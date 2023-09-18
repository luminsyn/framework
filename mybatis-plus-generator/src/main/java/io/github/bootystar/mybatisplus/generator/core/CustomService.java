package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.*;


/**
 * @Author booty
 * @Date 2023/8/21 9:44
 */
public interface CustomService<T,V> extends IService<T> {
    <S> V insertByDto(S o);
    <S> boolean insertBatchByDto(Collection<S> dtoList);
    <S> boolean updateByDto(S dto);
    V getVoById(Serializable id);
    <U> U getVoById(Serializable id, Class<U> clazz);
    <S> List<V> listByDto(S dto);
    <S,U> List<U> listByDto(S dto, Class<U> clazz);
    <S> IPage<V> pageByDto(S dto, Long current, Long size);
    <S,U> IPage<U> pageByDto(S dto, Long current, Long size, Class<U> clazz);
    <S,U> void exportExcel(S dto, OutputStream os, Class<U> clazz);
    <S,U> void exportExcel(S dto, OutputStream os, Class<U> clazz, Collection<String> includeFields);
    <U> boolean importExcel(InputStream is, Class<U> clazz);


    default T toEntity(Object source) {
        T t = null;
        try {
            ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
            Class<T> clazz = (Class) pt.getActualTypeArguments()[0];
            t = clazz.newInstance();
            Field[] fields = clazz.getDeclaredFields();
            Map<String, Object> fileldMap = this.toMap(source);
            for (Field field : fields) {
                field.setAccessible(true);
                String name = field.getName();
                Object o = fileldMap.get(name);
                if (o != null) {
                    field.set(t, o);
                }
            }
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
            v = clazz.newInstance();
            Field[] fields = clazz.getDeclaredFields();
            Map<String, Object> fileldMap = this.toMap(source);
            for (Field field : fields) {
                field.setAccessible(true);
                String name = field.getName();
                Object o = fileldMap.get(name);
                if (o != null) {
                    field.set(v, o);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return v;
    }



    default Map<String, Object> toMap(Object source) {
        if (source==null){
            return new HashMap<String, Object>();
        }

        if (source instanceof Map) {
            ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
            Class<T> clazz = (Class) pt.getActualTypeArguments()[0];
            if (String.class.equals(clazz)) {
                return (Map) source;
            } else {
                Map sourceMap = (Map) source;
                LinkedHashMap<String, Object> map = new LinkedHashMap<>();
                sourceMap.forEach((k, v) -> {
                    if (k != null && v != null) {
                        map.put(k.toString(), v);
                    }
                });
                return map;
            }
        }
        Map<String, Object> map = new LinkedHashMap<>();
        Class<?> clazz = source.getClass();
        Field[] fields = clazz.getDeclaredFields();
        try {
            for (Field field : fields) {
                field.setAccessible(true);
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


    default <U> U toTarget(Object source, Class<U> clazz) {
        U target = null;
        try {
            if (clazz==null){
                ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
                clazz = (Class) pt.getActualTypeArguments()[0];
            }
            target = clazz.newInstance();
            Field[] fields = clazz.getDeclaredFields();
            Map<String, Object> fileldMap = this.toMap(source);
            for (Field field : fields) {
                field.setAccessible(true);
                String name = field.getName();
                Object o = fileldMap.get(name);
                if (o != null) {
                    field.set(target, o);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return target;
    }
}
