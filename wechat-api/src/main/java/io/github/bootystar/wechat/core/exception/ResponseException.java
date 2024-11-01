package io.github.bootystar.wechat.core.exception;

import io.github.bootystar.wechat.core.ResponseBase;

/**
 * 微信返回值异常信息
 * @author bootystar
 *
 */
public class ResponseException extends RuntimeException{
    private ResponseBase responseBase;
    public ResponseException() {
    }

    public ResponseException(ResponseBase responseBase){
        super(responseBase.getErrmsg());
        this.responseBase=responseBase;
    }

    public ResponseBase getResponseBase() {
        return responseBase;
    }
}
