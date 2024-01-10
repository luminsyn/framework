package io.github.bootystar.wechat.core;

import io.github.bootystar.wechat.core.token.AccessToken;
import io.github.bootystar.wechat.core.token.AccessTokenFactory;
import io.github.bootystar.wechat.core.token.StableAccessTokenFactory;
import io.github.bootystar.wechat.core.token.factory.DefaultAccessTokenFactory;
import io.github.bootystar.wechat.core.token.factory.DefaultStableAccessTokenFactory;

/**
 * 基础api
 * @author booty
 *
 */
public class ApiBase {


    private final String appId;
    private final String appSecret;
    private AccessTokenFactory tokenFactory;
    private StableAccessTokenFactory stableTokenFactory;
    private boolean stableToken=true;



    public ApiBase(String appId, String appSecret) {
        this.appId = appId;
        this.appSecret = appSecret;
        this.tokenFactory=new DefaultAccessTokenFactory(appId,appSecret);
        this.stableTokenFactory=new DefaultStableAccessTokenFactory(appId,appSecret);
    }

    public String getAppId() {
        return appId;
    }

    protected String getAppSecret() {
        return appSecret;
    }

    public void setAccessTokenFactory(AccessTokenFactory tokenFactory) {
        this.tokenFactory = tokenFactory;
    }

    public void setStableTokenFactory(StableAccessTokenFactory stableTokenFactory) {
        this.stableTokenFactory = stableTokenFactory;
    }


    public boolean isStableToken() {
        return stableToken;
    }

    public void setStableToken(boolean stableToken) {
        this.stableToken = stableToken;
    }

    /**
     * 获取授权令牌的值
     * @return {@code AccessToken }
     * @author booty
     *
     */
    public String getTokenValue() {
        if(stableToken){
            getStableAccessToken().getAccess_token();
        }
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
        return tokenFactory.getAccessToken();
    }

    /**
     * 获取稳定访问令牌
     * 与获取Access token获取的调用凭证完全隔离，互不影响。
     *
     * @return {@link AccessToken }
     * @author booty
     *
     */
    public AccessToken getStableAccessToken() {
        return stableTokenFactory.getStableAccessToken();
    }




}
