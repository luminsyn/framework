package io.github.bootystar.wechat.officialAccount.entity;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.tool.ObjectTool;
import io.github.bootystar.wechat.common.exception.WechatResponseException;
import io.github.bootystar.wechat.entity.ResponseBase;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * 用户AccessToken
 * @author booty
 * @date 2023/6/4 10:25
 */
@Data
public class UserAccessToken extends ResponseBase {
    /**
     * 网页授权接口调用凭证,注意：此access_token与基础支持的access_token不同
     */
    private String access_token;
    /**
     * access_token接口调用凭证超时时间，单位（秒）
     */
    private Long expires_in;
    /**
     * 用户刷新access_token
     */
    private String refresh_token;
    /**
     * 用户唯一标识，请注意，在未关注公众号时，用户访问公众号的网页，也会产生一个用户和公众号唯一的OpenID
     */
    private String openid;
    /**
     * 用户授权的作用域，使用逗号（,）分隔
     */
    private String scope;
    /**
     * 是否为快照页模式虚拟账号，只有当用户是快照页模式虚拟账号时返回，值为1
     */
    private String is_snapshotuser;
    /**
     * 用户统一标识（针对一个微信开放平台帐号下的应用，同一用户的 unionid 是唯一的），
     * 只有当scope为"snsapi_userinfo"时返回
     */
    private String unionid;

    // ======================================

    /**
     * 过期时间
     */
    private LocalDateTime expiresTime;
    private static final String GET_PERSONAL_TOKEN_URL="https://api.weixin.qq.com/sns/oauth2/access_token?appid=APPID&secret=SECRET&code=CODE&grant_type=authorization_code";

    /**
     * 创建用户访问令牌
     *
     * 这里通过code换取的是一个特殊的网页授权access_token,
     * 与基础支持中的access_token（该access_token用于调用其他接口）不同。
     * 公众号可通过下述接口来获取网页授权access_token。
     * 如果网页授权的作用域为snsapi_base，则本步骤中获取到网页授权access_token的同时，也获取到了openid，snsapi_base式的网页授权流程即到此为止。
     *
     * 尤其注意：由于公众号的secret和获取到的access_token安全级别都非常高，必须只保存在服务器，不允许传给客户端。
     * 后续刷新access_token、通过access_token获取用户信息等步骤，也必须从服务器发起。
     *
     * @param appId     应用程序id
     * @param appSecret 应用程序秘密
     * @param code      代码
     * @return {@code UserAccessToken }
     * @author booty
     * @date 2023/06/08 09:16
     */
    public static UserAccessToken createUserAccessToken(String appId, String appSecret, String code){
        String url = GET_PERSONAL_TOKEN_URL.replace("APPID", appId).replace("SECRET", appSecret).replace("CODE",code);
        String result = HttpTool.doGet(url);
        /*
            {
              "access_token":"ACCESS_TOKEN",
              "expires_in":7200,
              "refresh_token":"REFRESH_TOKEN",
              "openid":"OPENID",
              "scope":"SCOPE",
              "is_snapshotuser": 1,
              "unionid": "UNIONID"
            }
         */
        UserAccessToken token = JSON.parseObject(result, UserAccessToken.class);
        if (ObjectTool.isEmpty(token.getAccess_token())){
            throw new WechatResponseException(token);
        }
        // 设置过期时间
        token.setExpiresTime(LocalDateTime.now().plusSeconds(token.getExpires_in()));
        return token;
    }

    private static final String GET_CHECK_PERSONAL_TOKEN_URL ="https://api.weixin.qq.com/sns/auth?access_token=ACCESS_TOKEN&openid=OPENID";


    /**
     * 检验授权凭证（access_token）是否有效
     * ResponseBase.errcode=0 时表示有效
     *
     * @param token  令牌
     * @param openId 开放id
     * @return {@code ResponseBase }
     * @author booty
     * @date 2023/06/08 10:51
     */
    public static ResponseBase checkUserAccessToken(String token,String openId){
        String url = GET_CHECK_PERSONAL_TOKEN_URL.replace("ACCESS_TOKEN", token).replace("OPENID",openId);
        String result = HttpTool.doGet(url);
        /*
        { "errcode":0,"errmsg":"ok"}
         */
        return JSON.parseObject(result, ResponseBase.class);
    }




    private static final String REFRESH_PERSONAL_TOKEN_URL="https://api.weixin.qq.com/sns/oauth2/refresh_token?appid=APPID&grant_type=refresh_token&refresh_token=REFRESH_TOKEN";

    /**
     * 刷新用户访问令牌
     * 由于access_token拥有较短的有效期，当access_token超时后，可以使用refresh_token进行刷新，
     * refresh_token有效期为30天，当refresh_token失效之后，需要用户重新授权
     *
     * @param appId    应用程序id
     * @param oldToken 旧令牌
     * @return {@code UserAccessToken }
     * @author booty
     * @date 2023/06/08 09:54
     */
    public static UserAccessToken refreshUserAccessToken(String appId,String oldToken){
        String url = GET_PERSONAL_TOKEN_URL.replace("APPID", appId).replace("REFRESH_TOKEN", oldToken);
        String result = HttpTool.doGet(url);
        /*
        {
          "access_token":"ACCESS_TOKEN",
          "expires_in":7200,
          "refresh_token":"REFRESH_TOKEN",
          "openid":"OPENID",
          "scope":"SCOPE"
        }
         */
        UserAccessToken token = JSON.parseObject(result, UserAccessToken.class);
        // 设置过期时间
        token.setExpiresTime(LocalDateTime.now().plusSeconds(token.getExpires_in()));
        if (ObjectTool.isEmpty(token.getAccess_token())){
//            String msg=String.format("wechat userToken required failed,response code:%s, responseMsg:%s",token.getErrcode(),token.getErrmsg());
            throw new WechatResponseException(token);
        }
        return token;
    }



}
