package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.util.*;


/**
 * @author booty
 * @since 2023/8/21 9:44
 */
public interface CustomService<T,V> extends IService<T> {
    <S> V insertByDto(S o);
    <S> boolean insertBatchByDto(Collection<S> dtoList);
    <S> boolean updateByDto(S dto);
    V getVoById(Serializable id);
    <U> U getVoById(Serializable id, Class<U> clazz);
    <S> V oneByDto(S dto);
    <S,U> U oneByDto(S dto, Class<U> clazz);
    <S> List<V> listByDto(S dto);
    <S,U> List<U> listByDto(S dto, Class<U> clazz);
    <S> IPage<V> pageByDto(S dto, Long current, Long size);
    <S,U> IPage<U> pageByDto(S dto, Long current, Long size, Class<U> clazz);
    <S,U> void exportExcel(S dto, OutputStream os, Class<U> clazz);
    <S,U> void exportExcel(S dto, OutputStream os, Class<U> clazz, Collection<String> includeFields);
    <U> boolean importExcel(InputStream is, Class<U> clazz);
    <U> void exportTemplate(OutputStream os, Class<U> clazz);


    default T toEntity(Object source) {
        T t = null;
        try {
            ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
            Class<T> clazz = (Class) pt.getActualTypeArguments()[0];
            t = toTarget(source, clazz);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return t;
    }

    default V toVo(Object source) {
        V v = null;
        try {
            ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
            Class<V> clazz = (Class) pt.getActualTypeArguments()[1];
            v = toTarget(source, clazz);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return v;
    }

    default <U> U toTarget(Object source, Class<U> clazz) {
        U target = null;
        try {
            if (clazz==null){
               return null;
            }
            target = clazz.newInstance();
            this.copyProperties(source, target);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return target;
    }



    default Map<String, Object> toMap(Object source) {
        if (source==null){
            return new HashMap<>();
        }
        if (source instanceof Map) {
            return new HashMap<>((Map) source);
        }
        Map<String, Object> map = new LinkedHashMap<>();
        Class<?> clazz = source.getClass();
        Field[] fields = clazz.getDeclaredFields();
        try {
            for (Field field : fields) {
                field.setAccessible(true);
                int modifiers = field.getModifiers();
                if (Modifier.isFinal(modifiers)){
                    continue;
                }
                if (Modifier.isStatic(modifiers)){
                    continue;
                }
                if (Modifier.isNative(modifiers)){
                    continue;
                }
                String key = field.getName();
                Object value = field.get(source);
                if (value != null) {
                    map.put(key, value);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return map;
    }



    default void copyProperties(Object source, Object target) {
       if (source==null || target==null){
           return;
       }
        Map<?, ?> map = this.toMap(source);
        Field[] declaredFields = target.getClass().getDeclaredFields();
        for (Field field : declaredFields) {
            field.setAccessible(true);
            int modifiers = field.getModifiers();
            if (Modifier.isFinal(modifiers)){
                continue;
            }
            if (Modifier.isStatic(modifiers)){
                continue;
            }
            if (Modifier.isNative(modifiers)){
                continue;
            }
            String name = field.getName();
            Object o = map.get(name);
            if (o != null) {
                try {
                    field.set(target, o);
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                }
            }
        }
    }


}
