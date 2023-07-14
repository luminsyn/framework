package io.github.bootystar.mybatisplus.generator.core.entity;

import org.springframework.beans.BeanUtils;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * @Author booty
 * @Date 2023/7/13 11:30
 */
public class Dto<T> {

    public T toBean(){
        ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
        Type[] typeArguments = pt.getActualTypeArguments();
        Class<T> clazz = (Class<T>) typeArguments[0];
        T t = null;
        try {
            t = clazz.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            e.printStackTrace();
        }
        BeanUtils.copyProperties(this,t);
        return t;
    }

}
