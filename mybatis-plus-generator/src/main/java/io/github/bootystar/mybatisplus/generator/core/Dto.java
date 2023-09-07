package io.github.bootystar.mybatisplus.generator.core;

import javax.swing.text.html.parser.Entity;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 基础dto
 * @Author booty
 * @Date 2023/8/21 9:03
 */
public class Dto<T> {

    public T toEntity() {
        T t = null;
        try {
            ParameterizedType pt = (ParameterizedType)this.getClass().getGenericSuperclass();
            Class<T> clazz = (Class)pt.getActualTypeArguments()[0];
            t = clazz.newInstance();
            Field[] fields = clazz.getDeclaredFields();
            Map<String, Object> fileldMap = this.toMap();
            for (Field field : fields) {
                field.setAccessible(true);
                String name = field.getName();
                Object o = fileldMap.get(name);
                if (o!= null) {
                    field.set(t,o);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return t;
    }

    public Map<String,Object> toMap() {
        Map<String,Object> map = new LinkedHashMap<>();
        Class<?> clazz = this.getClass();
        Field[] fields = clazz.getDeclaredFields();
        try {
            for (Field field : fields) {
                field.setAccessible(true);
                String key = field.getName();
                Object value = field.get(this);
                if (value != null) {
                    map.put(key, value);
                }
            }
        }catch (Exception e){
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return map;
    }





}
