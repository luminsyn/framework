package io.github.bootystar.mybatisplus.util;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.SneakyThrows;

import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.TypeVariable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * @author bootystar
 */
public abstract class ReflectUtil {


    @Data
    @AllArgsConstructor
    public static class LambdaMethod{
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
    public static LambdaMethod lambdaMethodInfo(SFunction<?, ?> methodReference,Class<?> parameterClass){
        String methodName = "";
        String fullClassName= "";
        try {
            Method lambdaMethod = methodReference.getClass().getDeclaredMethod("writeReplace");
            lambdaMethod.setAccessible(Boolean.TRUE);
            SerializedLambda serializedLambda  = (SerializedLambda) lambdaMethod.invoke(methodReference);
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
                if (!returnType.equals(clazz)){
                    throw new NoSuchMethodException("no method found return self");
                }
                int modifiers = returnMethod.getModifiers();
                if (Modifier.isStatic(modifiers)){
                    isStaticMethod=true;
                    methodNameFullStr=clazz.getSimpleName()+"."+methodNameFullStr;
                }else{
                    clazz.getConstructor();
                    methodNameFullStr="new "+clazz.getSimpleName()+"()."+methodNameFullStr;
                }
            }catch (NoSuchMethodException e){
                clazz.getConstructor(parameterClass);
                methodNameFullStr="new "+clazz.getSimpleName();
                if (isGenericTypeClass) {
                    methodNameFullStr += "<>";
                }
                isConstructor=true;
            }
            return new ReflectUtil.LambdaMethod(
                    clazz.getPackage().getName()
                    , clazz.getSimpleName()
                    , isGenericTypeClass
                    , methodName
                    , methodNameFullStr
                    , isStaticMethod
                    , isConstructor
            );
        }catch (Exception e){
            String msg= String.format("can't find constructor or method in class [%s] , method name [%s], parameter class [%s]",fullClassName,methodName,parameterClass.getName());
            throw new IllegalStateException(msg);
        }
    }

    /**
     * 新建实例
     *
     * @param clazz 克拉兹
     * @return {@link T }
     * @author bootystar
     */
    @SneakyThrows
    public static <T> T newInstance(Class<T> clazz){
        return clazz.getConstructor().newInstance();
    }

    /**
     * 指定类属性map
     *
     * @param clazz 类
     * @return {@link Map }<{@link String }, {@link Field }>
     * @author bootystar
     */
    public static Map<String, Field> fieldMap(Class<?> clazz){
        Map<String, Field> map = new HashMap<>();
        while (clazz!=null){
            Field[] fields = clazz.getDeclaredFields();
            for (Field field : fields) {
                field.setAccessible(true);
                int modifiers = field.getModifiers();
                if (Modifier.isStatic(modifiers)||Modifier.isFinal(modifiers)||Modifier.isNative(modifiers)) continue;
                map.putIfAbsent(field.getName(), field);
            }
            clazz = clazz.getSuperclass();
        }
        return map;
    }

    /**
     * 复制属性
     *
     * @param source 来源
     * @param target 目标
     * @return {@link T }
     * @author bootystar
     */
    @SneakyThrows
    public static <T> T copyProperties(Object source, T target) {
        if (source == null || target == null || source.equals(target)) return target;
        Map<String, Field> sourceMap = fieldMap(source.getClass());
        Map<String, Field> targetMap = fieldMap(target.getClass());
        for (Field field : sourceMap.values()) {
            Object o = field.get(source);
            if (o == null) continue;
            Field targetFiled = targetMap.get(field.getName());
            if (targetFiled != null && targetFiled.getType().isAssignableFrom(field.getType())) {
                targetFiled.set(target, o);
            }
        }
        return target;
    }


    /**
     * 对象转map
     *
     * @param source 来源
     * @return {@link Map }<{@link String },{@link Object }>
     * @author bootystar
     */
    @SneakyThrows
    public static Map<String,Object> objectToMap(Object source) {
        HashMap<String, Object> map = new HashMap<>();
        if (source == null ) return map;
        if (source instanceof Map) return (Map<String, Object>) source;
        Collection<Field> fields = fieldMap(source.getClass()).values();
        for (Field field : fields) {
            Object o = field.get(source);
            if (o == null) continue;
            map.put(field.getName(), o);
        }
        return map;
    }


    public static Map<String,String> fieldConvertMap(Class<?> clazz){
        // 张三' OR '1' = '1';truncate table 'user11
        Map<String, Field> fieldMap = fieldMap(clazz);
        Map<String, String> result = new HashMap<>();
        for (Field field : fieldMap.values()) {
            String fieldName = field.getName();
            String jdbcColumn= fieldName;
            TableLogic tableLogic = field.getAnnotation(TableLogic.class);
            if (tableLogic != null) {
                continue;
            }
            TableId tableId = field.getAnnotation(TableId.class);
            if (tableId != null) {
                String value = tableId.value();
                if (!value.isEmpty()) {
                    jdbcColumn= value;
                    if (!value.contains(".")){
                        jdbcColumn = String.format("a.`%s`", jdbcColumn);
                    }
                }
                result.put(fieldName, jdbcColumn);
                continue;
            }
            TableField tableField = field.getAnnotation(TableField.class);
            if (tableField != null) {
                boolean exist = tableField.exist();
                String value = tableField.value();
                if (!exist) {
                    if (value.isEmpty()){
                        continue;
                    }
                    result.put(fieldName, value);
                    continue;
                }
                if (!value.isEmpty()) {
                    jdbcColumn= value;
                    if (!value.contains(".")){
                        jdbcColumn = String.format("a.`%s`", jdbcColumn);
                    }
                }
                result.put(fieldName, jdbcColumn);
                continue;
            }
            result.put(fieldName, String.format("a.`%s`",jdbcColumn));
        }
        return result;
    }



}
