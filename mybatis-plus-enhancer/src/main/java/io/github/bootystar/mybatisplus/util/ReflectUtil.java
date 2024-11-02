package io.github.bootystar.mybatisplus.util;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.generator.BaseGenerator;
import io.github.bootystar.mybatisplus.injection.Injectable;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.SneakyThrows;

import java.lang.invoke.SerializedLambda;
import java.lang.reflect.*;
import java.util.*;
import java.util.function.Function;

/**
 * @author bootystar
 */
public abstract class ReflectUtil extends Reflector {


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
            return new ReflectUtil.LambdaMethod(
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
     * id字段
     *
     * @param clazz 克拉兹
     * @return {@link String }
     * @author bootystar
     */
    public static String idField(Class<?> clazz) {
        Map<String, Field> fieldMap = fieldMap(clazz);
        Collection<Field> values = fieldMap.values();
        ArrayList<String> fieldNames = new ArrayList<>();
        for (Field field : values) {
            String fieldName = field.getName();
            String jdbcColumn = fieldName;
            TableId tableId = field.getAnnotation(TableId.class);
            if (tableId != null) {
                String value = tableId.value();
                if (!value.isEmpty()) {
                    jdbcColumn = value;
                }
                return jdbcColumn;
            }
            fieldNames.add(fieldName);
        }
        return fieldNames.contains("id") ? "id" : null;
    }


    /**
     * 实体类与数据库字段转换映射
     *
     * @param clazz 克拉兹
     * @return {@link Map }<{@link String },{@link String }>
     * @author bootystar
     */
    public static Map<String, String> fieldConvertMap(Class<?> clazz) {
        // 张三' OR '1' = '1';truncate table 'user11
        Map<String, Field> fieldMap = fieldMap(clazz);
        Map<String, String> result = new HashMap<>();
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
                result.put(fieldName, jdbcColumn);
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
                    result.put(fieldName, value);
                    continue;
                }
                if (!value.isEmpty()) {
                    jdbcColumn = value;
                    if (!value.contains(".")) {
                        jdbcColumn = String.format("a.`%s`", jdbcColumn);
                    }
                }
                result.put(fieldName, jdbcColumn);
                continue;
            }
            result.put(fieldName, String.format("a.`%s`", jdbcColumn));
        }
        return result;
    }


    /**
     * 获取实体类字段映射
     * 当实体类实现{@link Injectable }接口后,会额外添加映射字段
     *
     * @param entityClass 实体类
     * @return {@link Map }<{@link String }, {@link String }>
     * @author bootystar
     */
    @SneakyThrows
    public static Map<String, String> injectableFieldsMap(Class<?> entityClass) {
        Map<String, String> map = ReflectUtil.fieldConvertMap(entityClass);
        if (Injectable.class.isAssignableFrom(entityClass)) {
            Class<Injectable> injectable = (Class<Injectable>) entityClass;
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





    public static void main(String[] args) {
        Type[] types = ReflectUtil.resolveTypeArguments(C.class);
        System.out.println();

        C<BaseGenerator, EnhanceService> c = new C<BaseGenerator, EnhanceService>();
        Type[] types1 = ReflectUtil.resolveTypeArguments(c.getClass());
        Type[] types2 = ReflectUtil.resolveTypeArguments(C.class, EnhanceService.class);

        System.out.println();
    }


    static class C<BaseGenerator,B> extends Test<BaseGenerator>{

    }

    static class Test<S> implements EnhanceService<A, B> {

        @Override
        public <S> List<B> doSelect(S s, IPage<B> page) {
            return Collections.emptyList();
        }

        @Override
        public boolean saveBatch(Collection<A> entityList, int batchSize) {
            return false;
        }

        @Override
        public boolean saveOrUpdateBatch(Collection<A> entityList, int batchSize) {
            return false;
        }

        @Override
        public boolean updateBatchById(Collection<A> entityList, int batchSize) {
            return false;
        }

        @Override
        public boolean saveOrUpdate(A entity) {
            return false;
        }

        @Override
        public A getOne(Wrapper<A> queryWrapper, boolean throwEx) {
            return null;
        }

        @Override
        public Optional<A> getOneOpt(Wrapper<A> queryWrapper, boolean throwEx) {
            return Optional.empty();
        }

        @Override
        public Map<String, Object> getMap(Wrapper<A> queryWrapper) {
            return Collections.emptyMap();
        }

        @Override
        public <V> V getObj(Wrapper<A> queryWrapper, Function<? super Object, V> mapper) {
            return null;
        }

        @Override
        public BaseMapper<A> getBaseMapper() {
            return null;
        }

        @Override
        public Class<A> getEntityClass() {
            return null;
        }
    }

    class A {

    }

    class B {

    }


}
