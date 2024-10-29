package io.github.bootystar.wechat.core;

import io.github.bootystar.wechat.core.token.AccessToken;
import io.github.bootystar.wechat.core.token.AccessTokenFactory;
import io.github.bootystar.wechat.core.token.StableAccessTokenFactory;
import io.github.bootystar.wechat.core.token.factory.DefaultAccessTokenFactory;
import io.github.bootystar.wechat.core.token.factory.DefaultStableAccessTokenFactory;
import lombok.Getter;
import lombok.Setter;

/**
 * 基础api
 * @author booty
 *
 */
public class ApiBase {


    @Getter
    protected final String appId;
    protected final String appSecret;
    protected AccessTokenFactory tokenFactory;
    @Setter
    protected StableAccessTokenFactory stableTokenFactory;
    @Setter
    @Getter
    protected boolean stableToken=true;



    public ApiBase(String appId, String appSecret) {
        this.appId = appId;
        this.appSecret = appSecret;
        this.tokenFactory=new DefaultAccessTokenFactory(appId,appSecret);
        this.stableTokenFactory=new DefaultStableAccessTokenFactory(appId,appSecret);
    }

    protected String getAppSecret() {
        return appSecret;
    }

    public void setAccessTokenFactory(AccessTokenFactory tokenFactory) {
        this.tokenFactory = tokenFactory;
    }


    /**
     * 获取授权令牌的值
     * @return {@code AccessToken }
     * @author booty
     *
     */
    public String getAccessTokenValue() {
        return getAccessToken().getAccess_token();
    }

    /**
     * 获取访问令牌
     *
     * @return {@code AccessToken }
     * @author booty
     *
     */
    public AccessToken getAccessToken(){
        if(stableToken){
            return stableTokenFactory.getStableAccessToken();
        }
        return tokenFactory.getAccessToken();
    }




}
