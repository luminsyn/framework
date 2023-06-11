package io.github.bootystar.wechat.officialAccount.core;

import io.github.bootystar.wechat.entity.ResponseBase;
import io.github.bootystar.wechat.officialAccount.entity.*;
import io.github.bootystar.wechat.officialAccount.enums.CgiPathEnum;
import io.github.bootystar.wechat.officialAccount.interfaces.AccessTokenFactory;
import lombok.SneakyThrows;

import java.net.URLEncoder;
import java.time.LocalDateTime;

/**
 * 微信公众号核心类
 * @author booty
 * @date 2023/6/4 10:28
 */
public class OfficialAccount {

    private AccessToken accessToken;
    private final String appId;
    private final String appSecret;
    public OfficialAccount(String appId, String appSecret) {
        this.appId = appId;
        this.appSecret = appSecret;
    }


    /**
     *     以snsapi_base为scope发起的网页授权，是用来获取进入页面的用户的openid的，并且是静默授权并自动跳转到回调页的。用户感知的就是直接进入了回调页（往往是业务页面）
     *     以snsapi_userinfo为scope发起的网页授权，是用来获取用户的基本信息的。但这种授权需要用户手动同意，并且由于用户同意过，所以无须关注，就可在授权后获取该用户的基本信息
     *     应用授权作用域，
     *     snsapi_base （不弹出授权页面，直接跳转，只能获取用户openid），
     *     snsapi_userinfo （弹出授权页面，可通过openid拿到昵称、性别、所在地。并且， 即使在未关注的情况下，只要用户授权，也能获取其信息 ）
     *     用户同意授权后
     *     如果用户同意授权，页面将跳转至 redirect_uri/?code=CODE&state=STATE
     */
    private final static String URL_OAUTH2_URL="https://open.weixin.qq.com/connect/oauth2/authorize?appid=APPID&redirect_uri=REDIRECT_URI&response_type=code&scope=SCOPE&state=STATE#wechat_redirect";
    public static final String SCOPE_SNSAPI_BASE = "snsapi_base";
    public static final String SCOPE_SNSAPI_USERINFO = "snsapi_userinfo";


    /**
     * 获取snsapi_base作用域的授权url
     *
     * @param RedirectURL 重定向url
     * @return {@code String }
     * @author booty
     * @date 2023/06/06 15:50
     */
    @SneakyThrows
    public String oAuth2UrlBase(String RedirectURL){
        String url = URL_OAUTH2_URL.replace("APPID", appId);
        url = url.replace("REDIRECT_URI", URLEncoder.encode(RedirectURL, "UTF-8"));
        url = url.replace("SCOPE", SCOPE_SNSAPI_BASE);
        url = url.replace("STATE", "");
        return url;
    }

    /**
     * 获取snsapi_userinfo作用域的授权url
     * ()
     *
     * @param RedirectURL 重定向url
     * @return {@code String }
     * @author booty
     * @date 2023/06/06 15:50
     */
    @SneakyThrows
    public String oAuth2UrlUserInfo(String RedirectURL){
        String url = URL_OAUTH2_URL.replace("APPID", appId);
        url = url.replace("REDIRECT_URI", URLEncoder.encode(RedirectURL, "UTF-8"));
        url = url.replace("SCOPE", SCOPE_SNSAPI_USERINFO);
        url = url.replace("STATE", "");
        return url;
    }

    /**
     * 获取指定作用域的授权url
     *
     * @param RedirectURL 重定向url
     * @param scope       范围
     * @param state       状态  重定向后会带上state参数，开发者可以填写a-zA-Z0-9的参数值，最多128字节
     * @return {@code String }
     * @author booty
     * @date 2023/06/06 16:03
     */
    @SneakyThrows
    public String oAuth2Url(String RedirectURL,String scope,String state){
        String url = URL_OAUTH2_URL.replace("APPID", appId);
        url = url.replace("REDIRECT_URI", URLEncoder.encode(RedirectURL, "UTF-8"));
        url = url.replace("SCOPE", scope);
        url = url.replace("STATE", state);
        return url;
    }


    /**
     * 获取个人访问令牌
     *
     * 这里通过code换取的是一个特殊的网页授权access_token,
     * 与基础支持中的access_token（该access_token用于调用其他接口）不同。
     * 公众号可通过下述接口来获取网页授权access_token。
     * 如果网页授权的作用域为snsapi_base，则本步骤中获取到网页授权access_token的同时，也获取到了openid，snsapi_base式的网页授权流程即到此为止。
     *
     * 尤其注意：由于公众号的secret和获取到的access_token安全级别都非常高，必须只保存在服务器，不允许传给客户端。
     * 后续刷新access_token、通过access_token获取用户信息等步骤，也必须从服务器发起。
     *
     * @param code 微信网页重定向获取的授权code
     * @return {@code UserAccessToken }
     * @author booty
     * @date 2023/06/08 09:14
     */
    public UserAccessToken createPersonalAccessToken(String code){
        return UserAccessToken.createUserAccessToken(appId, appSecret, code);
    }

    /**
     * 检查个人访问令牌是否有效
     *
     * @param token  令牌
     * @param openId 开放id
     * @return boolean true 有效
     * @author booty
     * @date 2023/06/08 11:26
     */
    public boolean checkPersonalAccessToken(UserAccessToken token,String openId){
        ResponseBase responseBase = UserAccessToken.checkUserAccessToken(token.getAccess_token(), openId);
        return responseBase.getErrcode()==0;
    }



    /**
     * 刷新个人访问令牌
     * 由于access_token拥有较短的有效期，当access_token超时后，可以使用refresh_token进行刷新，
     * refresh_token有效期为30天，当refresh_token失效之后，需要用户重新授权
     *
     * @param oldToken 旧令牌
     * @return {@code UserAccessToken }
     * @author booty
     * @date 2023/06/08 10:22
     */
    public UserAccessToken refreshPersonalAccessToken(UserAccessToken oldToken){
        return UserAccessToken.refreshUserAccessToken(appId, oldToken.getRefresh_token());
    }

    /**
     * 拉取用户信息
     *
     * @param token  个人访问令牌
     * @param openId 开放id
     * @return {@code UserInfo }
     * @author booty
     * @date 2023/06/08 17:10
     */
    public UserInfo getUserInfoByOpenId(UserAccessToken token, String openId){
        return UserInfo.getUserInfoByOpenId(token.getAccess_token(),openId);
    }


    /**
     * 令牌工厂
     */
    private AccessTokenFactory accessTokenFactory;

    public void setAccessTokenFactory(AccessTokenFactory accessTokenFactory) {
        this.accessTokenFactory = accessTokenFactory;
    }


    /**
     * 创建访问令牌
     *
     * @return {@link AccessToken }
     * @author booty
     * @date 2023/06/11 14:10
     */
    private AccessToken createAccessToken(){
        if (this.accessTokenFactory !=null){
            return this.accessTokenFactory.createAccessToken();
        }
        return AccessToken.createAccessToken(appId, appSecret);
    }


    /**
     * 获取授权令牌
     * @return {@code AccessToken }
     * @author booty
     * @date 2023/06/06 15:31
     */
    public AccessToken getAccessToken() {
        if (this.accessToken==null){
            this.accessToken= createAccessToken();
        }
        if (LocalDateTime.now().isAfter(accessToken.getExpiresTime())){
            this.accessToken= createAccessToken();
        }
        return accessToken;
    }


    /**
     * 获取稳定访问令牌
     * 与获取Access token获取的调用凭证完全隔离，互不影响。
     *
     * @param refresh 是否强制更新令牌，若否，相同时间段内获取的为同一token
     * @return {@link AccessToken }
     * @author booty
     * @date 2023/06/11 18:26
     */
    public AccessToken getStableAccessToken(boolean refresh) {
        this.accessToken= AccessToken.createStableAccessToken(appId, appSecret,refresh);
        return accessToken;
    }


    /**
     * 使用AccessToken清空公众号/小程序等接口的每日调用接口次数
     *
     * @return boolean
     * @author booty
     * @date 2023/06/08 17:50
     */
    public boolean clearQuotaByAccessToken(){
        AccessToken accessToken = getAccessToken();
        ResponseBase responseBase = QueryQuota.clearQuotaByAccessToken(appId, accessToken.getAccess_token());
        return responseBase.getErrcode()==0;
    }


    /**
     * 使用appSecret清空公众号/小程序等接口的每日调用接口次数
     *
     * @return boolean
     * @author booty
     * @date 2023/06/08 17:51
     */
    public boolean clearQuotaByAppSecret(){
        ResponseBase responseBase = QueryQuota.clearQuotaByAppSecret(appId, appSecret);
        return responseBase.getErrcode()==0;
    }

    /**
     * 查询指定链接接口的调用次数
     * ”/xxx/sns/xxx“这类接口不支持使用该接口，会出现76022报错
     *
     * @param pathEnum 路径枚举
     * @return {@link QueryQuota }
     * @author booty
     * @date 2023/06/11 14:46
     */
    public QueryQuota queryQuota(CgiPathEnum pathEnum){
        AccessToken accessToken = getAccessToken();
        return QueryQuota.queryQuota(accessToken.getAccess_token(),pathEnum.url);
    }


    /**
     * 查询指定链接接口的调用次数
     * api的请求地址，例如"/cgi-bin/message/custom/send";不要前缀“https://api.weixin.qq.com” ，也不要漏了"/",否则都会76003的报错
     * @param cgiPath cgi路径
     * @return {@link QueryQuota }
     * @author booty
     * @date 2023/06/11 14:45
     */
    public QueryQuota queryQuota(String cgiPath){
        AccessToken accessToken = getAccessToken();
        return QueryQuota.queryQuota(accessToken.getAccess_token(),cgiPath);
    }


    /**
     * 查询rid对应的错误详情
     *
     * @param rid 掉
     * @return {@code Rid }
     * @author booty
     * @date 2023/06/08 18:10
     */
    public Rid queryRid(String rid){
        AccessToken accessToken = getAccessToken();
        return Rid.queryRid(accessToken.getAccess_token(),rid);
    }





}
