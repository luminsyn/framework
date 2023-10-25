package io.github.bootystar.wechat.core;


import java.io.Serializable;

/**
 * 微信返回错误代码
 * @author booty
 * @since 2023/6/8 9:43
 */
public class ResponseBase implements Serializable {
    private static final long serialVersionUID = 1L;

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
