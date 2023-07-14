package io.github.bootystar.mybatisplus.generator.core.entity;

import org.springframework.beans.BeanUtils;


/**
 * Vo
 * @Author booty
 * @Date 2023/7/13 10:52
 */
public class Vo<T> {

    public static <T> Vo<T> fromBean(T t){
        Vo<T> vo = new Vo<>();
        BeanUtils.copyProperties(t,vo);
        return vo;
    }



}
