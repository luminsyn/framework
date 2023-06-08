package io.github.bootystar.wechat.entity;


/**
 * 微信返回错误代码
 * @Author booty
 * @Date 2023/6/8 9:43
 */

public  class ResponseBase {

    private Integer errcode;
    private String errmsg;

    public Integer getErrcode() {
        return errcode;
    }

    public void setErrcode(Integer errcode) {
        this.errcode = errcode;
    }

    public String getErrmsg() {
        return errmsg;
    }

    public void setErrmsg(String errmsg) {
        this.errmsg = errmsg;
    }
}
