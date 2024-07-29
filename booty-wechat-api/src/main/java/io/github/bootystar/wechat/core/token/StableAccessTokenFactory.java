package io.github.bootystar.wechat.core.token;

/**
 * @author booty
 *
 */
@FunctionalInterface
public interface StableAccessTokenFactory {

    /**
     * 得到稳定访问令牌
     *
     * @return {@code AccessToken }
     * @author booty
     *
     */
    AccessToken getStableAccessToken();
}
