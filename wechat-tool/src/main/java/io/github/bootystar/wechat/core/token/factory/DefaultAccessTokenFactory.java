package io.github.bootystar.wechat.core.token.factory;

import io.github.bootystar.wechat.core.token.AccessToken;
import io.github.bootystar.wechat.core.token.AccessTokenFactory;

import java.time.LocalDateTime;

/**
 * 默认令牌工厂
 * @author bootystar
 * 
 */
public class DefaultAccessTokenFactory implements AccessTokenFactory {

    private AccessToken accessToken;

    private String appId;
    private String appSecret;

    public DefaultAccessTokenFactory(String appId, String appSecret) {
        this.appId = appId;
        this.appSecret = appSecret;
    }

    @Override
    public AccessToken getAccessToken() {
        if (accessToken==null || LocalDateTime.now().isAfter(accessToken.getExpiresTime())){
            accessToken = AccessToken.createAccessToken(appId,appSecret);
        }
        return accessToken;
    }
}
