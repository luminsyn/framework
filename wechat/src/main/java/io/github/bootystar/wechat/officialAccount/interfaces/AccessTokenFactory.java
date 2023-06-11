package io.github.bootystar.wechat.officialAccount.interfaces;

import io.github.bootystar.wechat.officialAccount.entity.AccessToken;

/**
 * 令牌创建方式
 * @author booty
 * @date 2023/6/11 14:05
 */
public interface AccessTokenFactory {

    /**
     * 创建访问令牌
     *
     * @return {@link AccessToken }
     * @author booty
     * @date 2023/06/11 14:05
     */
    AccessToken createAccessToken();
}
