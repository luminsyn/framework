package io.github.bootystar.mybatisplus.util;

import lombok.SneakyThrows;

import java.lang.reflect.*;
import java.util.*;

/**
 * @author bootystar
 */
public abstract class Reflector {

    /**
     * 新建实例
     *
     * @param clazz 克拉兹
     * @return {@link T }
     * @author bootystar
     */
    @SneakyThrows
    public static <T> T newInstance(Class<T> clazz) {
        return clazz.getConstructor().newInstance();
    }

    public static List<Class<?>> getParentClasses(Class<?> clazz) {
        List<Class<?>> list = new ArrayList<>();
        while (clazz != null && Object.class != clazz) {
            list.add(clazz);
            clazz = clazz.getSuperclass();
        }
        return list;
    }

    /**
     * 指定类属性map
     *
     * @param clazz 类
     * @return {@link Map }<{@link String }, {@link Field }>
     * @author bootystar
     */
    public static Map<String, Field> fieldMap(Class<?> clazz) {
        Map<String, Field> map = new HashMap<>();
        while (clazz != null && Object.class != clazz) {
            Field[] fields = clazz.getDeclaredFields();
            for (Field field : fields) {
                field.setAccessible(true);
                int modifiers = field.getModifiers();
                if (Modifier.isStatic(modifiers) || Modifier.isFinal(modifiers) || Modifier.isNative(modifiers))
                    continue;
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
    public static Map<String, Object> objectToMap(Object source) {
        HashMap<String, Object> map = new HashMap<>();
        if (source == null) return map;
        if (source instanceof Map) return (Map<String, Object>) source;
        Collection<Field> fields = fieldMap(source.getClass()).values();
        for (Field field : fields) {
            Object o = field.get(source);
            if (o == null) continue;
            map.put(field.getName(), o);
        }
        return map;
    }

    public static Type[] resolveTypeArguments(Class<?> clazz) {
        return resolveTypeArguments(clazz, null);
    }


    public static Type[] resolveTypeArguments(Class<?> clazz, Class<?> parent) {
        if (clazz == null) {
            throw new IllegalStateException("clazz cannot be null");
        }
        if (parent == null) {
            TypeVariable<? extends Class<?>>[] parameters = clazz.getTypeParameters();
            if (parameters.length > 0) {
                return parameters;
            }
            throw new IllegalStateException("No type parameters found in " + clazz.getName());
        }

        boolean anInterface = parent.isInterface();
        if (anInterface) {
            Type[] interfaces = clazz.getGenericInterfaces();
            for (Type t : interfaces) {
                if (t instanceof ParameterizedType) {
                    ParameterizedType pt = (ParameterizedType) t;
                    Type rawType = pt.getRawType();
                    if (rawType.equals(parent)) {
                        Type[] arguments = pt.getActualTypeArguments();
                        if (arguments == null || arguments.length == 0) {
                            throw new IllegalStateException(clazz.getName() + " No type parameters found for " + parent.getName());
                        }
                        return arguments;
                    }
                }
            }
        }
        boolean assignableFrom = parent.isAssignableFrom(clazz);
        if (!assignableFrom) {
            throw new IllegalStateException(clazz + " is not a child of " + parent);
        }
        // todo 递归搜索父类泛型实现
        List<Class<?>> classes = getParentClasses(clazz);
        for (Class<?> c : classes) {
            if (parent.isAssignableFrom(c)) {
                Type genericSuperclass = c.getGenericSuperclass();
                if (genericSuperclass instanceof ParameterizedType) {
                    ParameterizedType parameterizedType = (ParameterizedType) genericSuperclass;
                    boolean equals = parameterizedType.getRawType().equals(parent);
                    if (equals) {
                        Type[] arguments = parameterizedType.getActualTypeArguments();
                        if (arguments != null && arguments.length > 0) {
                            return arguments;
                        }
                    }
                }
            }
        }
        throw new IllegalStateException(clazz.getName() + " No type parameters found for " + parent.getName());
    }

}
