package io.github.bootystar.wechat.core.token;

/**
 * @author booty
 * @since 2023/6/16 15:35
 */
@FunctionalInterface
public interface StableAccessTokenFactory {

    /**
     * 得到稳定访问令牌
     *
     * @return {@code AccessToken }
     * @author booty
     * @since 2023/06/15 16:05
     */
    AccessToken getStableAccessToken();
}
