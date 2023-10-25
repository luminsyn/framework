package io.github.bootystar.wechat.core.token;

/**
 * 令牌创建方式
 * @author booty
 * @since 2023/6/11 14:05
 */
@FunctionalInterface
public interface AccessTokenFactory {


    /**
     * 获取访问令牌
     *
     * @return {@code AccessToken }
     * @author booty
     * @since 2023/06/15 16:06
     */
    AccessToken getAccessToken();



}
