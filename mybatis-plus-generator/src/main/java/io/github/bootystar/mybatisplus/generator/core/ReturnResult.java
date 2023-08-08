package io.github.bootystar.mybatisplus.generator.core;

import lombok.Data;

/**
 * 通用返回类型
 * @Author booty
 * @Date 2023/7/13 11:03
 */
@Data
public class ReturnResult<T> {
    public static final Integer SUCCESS = 1;
    public static final Integer FAILURE = 2;
    public static final Integer ERROR = 3;

    private Integer code;
    private String msg;
    private T data;

    public static <T> ReturnResult<T> success(String msg, T data) {
        ReturnResult<T> result = new ReturnResult<>();
        result.setCode(SUCCESS);
        result.setMsg(msg);
        result.setData(data);
        return result;
    }

    public static <T> ReturnResult<T> success(T data) {
        ReturnResult<T> result = new ReturnResult<>();
        result.setCode(SUCCESS);
        result.setData(data);
        return result;
    }

    public static <T> ReturnResult<T> failure(String msg) {
        ReturnResult<T> result = new ReturnResult<>();
        result.setCode(FAILURE);
        result.setMsg(msg);
        return result;
    }

    public static <T> ReturnResult<T> failure(String msg, T data) {
        ReturnResult<T> result = new ReturnResult<>();
        result.setCode(FAILURE);
        result.setMsg(msg);
        result.setData(data);
        return result;
    }

    public static <T> ReturnResult<T> error(String msg) {
        ReturnResult<T> result = new ReturnResult<>();
        result.setCode(ERROR);
        result.setMsg(msg);
        return result;
    }

    public static <T> ReturnResult<T> error(String msg, T data) {
        ReturnResult<T> result = new ReturnResult<>();
        result.setCode(ERROR);
        result.setMsg(msg);
        result.setData(data);
        return result;
    }

}
