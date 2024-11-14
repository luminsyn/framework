package io.github.bootystar.mybatisplus.util;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.core.metadata.TableFieldInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.logic.splicing.SplicingEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
import org.springframework.core.GenericTypeResolver;

import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.TypeVariable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * mybatis-plus解析工具类
 * @author bootystar
 */
public abstract class ReflectHelper4MybatisPlus extends ReflectHelper {

    @Data
    @AllArgsConstructor
    public static class LambdaMethod {
        private String classPackage;
        private String classSimpleName;
        private boolean isGenericTypeClass;
        private String methodName;
        private String methodNameFullStr;
        private boolean isStaticMethod;
        private boolean isConstructor;
    }

    /**
     * lambda方法信息
     *
     * @param methodReference lambda方法引用
     * @param parameterClass  参数类型
     * @return {@link LambdaMethod }
     * @author bootystar
     */
    public static LambdaMethod lambdaMethodInfo(SFunction<?, ?> methodReference, Class<?> parameterClass) {
        String methodName = "";
        String fullClassName = "";
        try {
            Method lambdaMethod = methodReference.getClass().getDeclaredMethod("writeReplace");
            lambdaMethod.setAccessible(Boolean.TRUE);
            SerializedLambda serializedLambda = (SerializedLambda) lambdaMethod.invoke(methodReference);
            fullClassName = serializedLambda.getImplClass().replace("/", ".");
            methodName = serializedLambda.getImplMethodName();
            String methodNameFullStr = methodName;
            Class<?> clazz = Class.forName(fullClassName);
            TypeVariable<? extends Class<?>>[] typeParameters = clazz.getTypeParameters();
            boolean isStaticMethod = false;
            boolean isConstructor = false;
            boolean isGenericTypeClass = typeParameters.length > 0;
            try {
                Method returnMethod = clazz.getMethod(methodNameFullStr, parameterClass);
                Class<?> returnType = returnMethod.getReturnType();
                if (!returnType.equals(clazz)) {
                    throw new NoSuchMethodException("no method found return self");
                }
                int modifiers = returnMethod.getModifiers();
                if (Modifier.isStatic(modifiers)) {
                    isStaticMethod = true;
                    methodNameFullStr = clazz.getSimpleName() + "." + methodNameFullStr;
                } else {
                    clazz.getConstructor();
                    methodNameFullStr = "new " + clazz.getSimpleName() + "()." + methodNameFullStr;
                }
            } catch (NoSuchMethodException e) {
                clazz.getConstructor(parameterClass);
                methodNameFullStr = "new " + clazz.getSimpleName();
                if (isGenericTypeClass) {
                    methodNameFullStr += "<>";
                }
                isConstructor = true;
            }
            return new ReflectHelper4MybatisPlus.LambdaMethod(
                    clazz.getPackage().getName()
                    , clazz.getSimpleName()
                    , isGenericTypeClass
                    , methodName
                    , methodNameFullStr
                    , isStaticMethod
                    , isConstructor
            );
        } catch (Exception e) {
            String msg = String.format("can't find constructor or method in class [%s] , method name [%s], parameter class [%s]", fullClassName, methodName, parameterClass.getName());
            throw new IllegalStateException(msg);
        }
    }

    /**
     * id字段属性名
     *
     * @param clazz 克拉兹
     * @return {@link String }
     * @author bootystar
     */
    public static String idFieldPropertyName(Class<?> clazz) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        if (tableInfo == null) {
            return null;
        }
        return tableInfo.getKeyProperty();
    }




    /**
     * 实体类与数据库字段转换映射
     *
     * @param clazz 克拉兹
     * @return {@link Map }<{@link String },{@link String }>
     * @author bootystar
     */
    public static Map<String, String> fieldConvertMap(Class<?> clazz) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        List<TableFieldInfo> fieldList = tableInfo.getFieldList();
        Map<String, String> result = new HashMap<>();
        for (TableFieldInfo fieldInfo : fieldList) {
            Field field = fieldInfo.getField();
            String fieldName = field.getName();
            String jdbcColumn = fieldInfo.getColumn();
            result.put(fieldName, String.format("a.`%s`", jdbcColumn));
        }
        TableFieldInfo logicDeleteFieldInfo = tableInfo.getLogicDeleteFieldInfo();
        if (logicDeleteFieldInfo != null) {
            String name = logicDeleteFieldInfo.getField().getName();
            result.remove(name);
        }
        
        Map<String, Field> fieldMap = fieldMap(clazz);
        for (Field field : fieldMap.values()) {
            String fieldName = field.getName();
            String jdbcColumn = fieldName;
            TableLogic tableLogic = field.getAnnotation(TableLogic.class);
            if (tableLogic != null) {
                continue;
            }
            TableId tableId = field.getAnnotation(TableId.class);
            if (tableId != null) {
                String value = tableId.value();
                if (!value.isEmpty()) {
                    jdbcColumn = value;
                    if (!value.contains(".")) {
                        jdbcColumn = String.format("a.`%s`", jdbcColumn);
                    }
                }
                result.putIfAbsent(fieldName, jdbcColumn);
                continue;
            }
            TableField tableField = field.getAnnotation(TableField.class);
            if (tableField != null) {
                boolean exist = tableField.exist();
                String value = tableField.value();
                if (!exist) {
                    if (value.isEmpty()) {
                        continue;
                    }
                    result.putIfAbsent(fieldName, value);
                    continue;
                }
                if (!value.isEmpty()) {
                    jdbcColumn = value;
                    if (!value.contains(".")) {
                        jdbcColumn = String.format("a.`%s`", jdbcColumn);
                    }
                }
                result.putIfAbsent(fieldName, jdbcColumn);
                continue;
            }
            result.putIfAbsent(fieldName, String.format("a.`%s`", jdbcColumn));
        }
        return result;
    }


    /**
     * 获取实体类字段映射
     * 当实体类实现{@link SplicingEntity }接口后,会额外添加映射字段
     *
     * @param entityClass 实体类
     * @return {@link Map }<{@link String }, {@link String }>
     * @author bootystar
     */
    @SneakyThrows
    public static Map<String, String> injectableFieldsMap(Class<?> entityClass) {
        Map<String, String> map = fieldConvertMap(entityClass);
        if (SplicingEntity.class.isAssignableFrom(entityClass)) {
            Class<SplicingEntity> injectable = (Class<SplicingEntity>) entityClass;
            Map<String, String> extraMap = injectable.getConstructor().newInstance().extraMap();
            if (extraMap != null && !extraMap.isEmpty()) {
                Iterator<Map.Entry<String, String>> it = extraMap.entrySet().iterator();
                while (it.hasNext()) {
                    Map.Entry<String, String> next = it.next();
                    String fieldName = next.getKey();
                    String jdbcColumn = next.getValue();
                    if (jdbcColumn == null || jdbcColumn.isEmpty()) {
                        continue;
                    }
                    if (!jdbcColumn.contains(".")) {
                        jdbcColumn = String.format("a.`%s`", jdbcColumn);
                    }
                    map.put(fieldName, jdbcColumn);
                }
            }
        }
        return map;
    }


    /**
     * 解析超类泛型参数
     *
     * @param clazz      指定类
     * @param superClass 超类
     * @return {@link Class }
     * @author bootystar
     */
    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> superClass) {
        return GenericTypeResolver.resolveTypeArguments(clazz, superClass);
    }
}
