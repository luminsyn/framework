package io.github.bootystar.mybatisplus.generator.core;

/**
 * 通用返回类型
 * @Author booty
 * @Date 2023/7/13 11:03
 */
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


    public ReturnResult() {

    }

    public ReturnResult(Integer code, String msg, T data) {
        this.code = code;
        this.msg = msg;
        this.data = data;
    }


    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMsg() {
        return msg;
    }

    public void setMsg(String msg) {
        this.msg = msg;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

}
