package io.github.bootystar.wechat.core.token;

/**
 * 令牌创建方式
 * @author booty
 * 
 */
@FunctionalInterface
public interface AccessTokenFactory {


    /**
     * 获取访问令牌
     *
     * @return {@code AccessToken }
     * @author booty
     *
     */
    AccessToken getAccessToken();



}
