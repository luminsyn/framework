package io.github.bootystar.wechat.officialAccount.core;

import io.github.bootystar.wechat.officialAccount.entity.AccessToken;

/**
 * 微信公众号核心类
 * @author booty
 * @date 2023/6/4 10:28
 */
public class WechatOfficialAccount {

    private AccessToken accessToken;
    private String appId;
    private String appSecret;


    public WechatOfficialAccount(String appId, String appSecret) {
        this.appId = appId;
        this.appSecret = appSecret;
    }





}
