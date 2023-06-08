package io.github.bootystar.wechat.common.exception;

import io.github.bootystar.wechat.entity.ResponseBase;

/**
 * 微信返回值异常信息
 * @Author booty
 * @Date 2023/6/8 10:05
 */
public class WechatResponseException extends RuntimeException{
    private ResponseBase responseBase;
    public WechatResponseException() {
    }

    public WechatResponseException(ResponseBase responseBase){
        super(responseBase.getErrmsg());
        this.responseBase=responseBase;
    }

    public ResponseBase getResponseBase() {
        return responseBase;
    }
}
