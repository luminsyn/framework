package io.github.bootystar.wechat.core.token.factory;

import io.github.bootystar.wechat.core.token.AccessToken;
import io.github.bootystar.wechat.core.token.StableAccessTokenFactory;

import java.time.LocalDateTime;

/**
 * 默认稳定令牌工厂
 * @author bootystar
 * 
 */
public class DefaultStableAccessTokenFactory implements StableAccessTokenFactory {

    private AccessToken stableAccessToken;

    private String appId;
    private String appSecret;

    public DefaultStableAccessTokenFactory(String appId, String appSecret) {
        this.appId = appId;
        this.appSecret = appSecret;
    }

    @Override
    public AccessToken getStableAccessToken() {
        if (stableAccessToken==null || LocalDateTime.now().isAfter(stableAccessToken.getExpiresTime())){
            stableAccessToken = AccessToken.createStableAccessToken(appId,appSecret,false);
        }
        return stableAccessToken;
    }
}
