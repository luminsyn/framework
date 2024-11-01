package io.github.bootystar.wechat.core;


import io.github.bootystar.wechat.core.exception.ResponseException;
import lombok.Data;

import java.io.Serializable;

/**
 * 微信返回错误代码
 * @author bootystar
 *
 */
@Data
public class ResponseBase implements Serializable {
    private static final long serialVersionUID = 1L;

    private Integer errcode;
    private String errmsg;

    public void check(){
        if (getErrcode()!=null && getErrcode()!=0){
            throw new ResponseException(this);
        }
    }
}
