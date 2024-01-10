package io.github.bootystar.wechat.officialAccount;

import io.github.bootystar.wechat.core.ApiBase;
import io.github.bootystar.wechat.core.ResponseBase;
import io.github.bootystar.wechat.officialAccount.module.message.MessageApi;
import io.github.bootystar.wechat.officialAccount.module.message.ResponseIndustry;
import io.github.bootystar.wechat.officialAccount.module.menu.MenuBase;
import io.github.bootystar.wechat.officialAccount.module.menu.MenuApi;
import io.github.bootystar.wechat.officialAccount.module.menu.ResponseMenuQuery;
import io.github.bootystar.wechat.officialAccount.module.openApi.OpenApi;
import io.github.bootystar.wechat.officialAccount.module.openApi.ResponseQueryQuota;
import io.github.bootystar.wechat.officialAccount.module.openApi.ResponseQueryRid;
import io.github.bootystar.wechat.officialAccount.enums.CgiPathEnum;
import io.github.bootystar.wechat.officialAccount.module.web.UserAccessToken;
import io.github.bootystar.wechat.officialAccount.module.web.UserInfo;
import io.github.bootystar.wechat.officialAccount.module.web.WebApi;
import lombok.SneakyThrows;


/**
 * 微信公众号api
 * @author booty
 *
 */
public class OfficialAccountApi extends ApiBase {

    public OfficialAccountApi(String appId, String appSecret) {
        super(appId, appSecret);
    }




    /**
     * 获取snsapi_base作用域的授权url
     *
     * @param redirectURL 重定向url
     * @return {@code String }
     * @author booty
     *
     */
    public String oAuth2UrlBase(String redirectURL){
        return WebApi.oAuth2UrlBase(getAppId(),redirectURL);
    }

    /**
     * 获取snsapi_userinfo作用域的授权url
     *
     * @param redirectURL 重定向url
     * @return {@code String }
     * @author booty
     *
     */
    @SneakyThrows
    public String oAuth2UrlUserInfo(String redirectURL){
        return WebApi.oAuth2UrlUserInfo(getAppId(),redirectURL);
    }

    /**
     * 获取指定作用域的授权url
     *
     * @param redirectURL 重定向url
     * @param scope       范围
     * @param state       状态  重定向后会带上state参数，开发者可以填写a-zA-Z0-9的参数值，最多128字节
     * @return {@code String }
     * @author booty
     *
     */
    @SneakyThrows
    public String oAuth2Url(String redirectURL,String scope,String state){
        return WebApi.oAuth2Url(getAppId(),redirectURL,scope,state);
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
     *
     */
    public UserAccessToken createPersonalAccessToken(String code){
        return WebApi.createUserAccessToken(getAppId(), getAppSecret(), code);
    }

    /**
     * 检查个人访问令牌是否有效
     *
     * @param token  令牌
     * @param openId 开放id
     * @return boolean true 有效
     * @author booty
     *
     */
    public boolean checkPersonalAccessToken(UserAccessToken token,String openId){
        ResponseBase responseBase = WebApi.checkUserAccessToken(token.getAccess_token(), openId);
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
     *
     */
    public UserAccessToken refreshPersonalAccessToken(UserAccessToken oldToken){
        return WebApi.refreshUserAccessToken(getAppId(), oldToken.getRefresh_token());
    }

    /**
     * 拉取用户信息
     *
     * @param token  个人访问令牌
     * @param openId 开放id
     * @return {@code UserInfo }
     * @author booty
     *
     */
    public UserInfo getUserInfoByOpenId(UserAccessToken token, String openId){
        return WebApi.getUserInfoByOpenId(token.getAccess_token(),openId);
    }



    /**
     * 使用AccessToken清空公众号/小程序等接口的每日调用接口次数
     *
     * @return boolean
     * @author booty
     *
     */
    public boolean clearQuotaByAccessToken(){
        ResponseBase responseBase = OpenApi.clearQuotaByAccessToken(getAppId(), getTokenValue());
        return responseBase.getErrcode()==0;
    }


    /**
     * 使用appSecret清空公众号/小程序等接口的每日调用接口次数
     *
     * @return boolean
     * @author booty
     *
     */
    public boolean clearQuotaByAppSecret(){
        ResponseBase responseBase = OpenApi.clearQuotaByAppSecret(getAppId(), getAppSecret());
        return responseBase.getErrcode()==0;
    }

    /**
     * 查询指定链接接口的调用次数
     * ”/xxx/sns/xxx“这类接口不支持使用该接口，会出现76022报错
     *
     * @param pathEnum 路径枚举
     * @return {@link ResponseQueryQuota }
     * @author booty
     *
     */
    public ResponseQueryQuota queryQuota(CgiPathEnum pathEnum){
        return OpenApi.queryQuota(getTokenValue(),pathEnum.url);
    }


    /**
     * 查询指定链接接口的调用次数
     * api的请求地址，例如"/cgi-bin/message/custom/send";不要前缀“https://api.weixin.qq.com” ，也不要漏了"/",否则都会76003的报错
     * @param cgiPath cgi路径
     * @return {@link ResponseQueryQuota }
     * @author booty
     *
     */
    public ResponseQueryQuota queryQuota(String cgiPath){
        return OpenApi.queryQuota(getTokenValue(),cgiPath);
    }


    /**
     * 查询rid对应的错误详情
     *
     * @param rid 掉
     * @return {@code Rid }
     * @author booty
     *
     */
    public ResponseQueryRid queryRid(String rid){
        return OpenApi.queryRid(getTokenValue(),rid);
    }


    /**
     * 创建/调整菜单内容
     *
     * @param menu 菜单
     * @return {@code ResponseBase }
     * @author booty
     *
     */
    public ResponseBase adjustMenu(MenuBase menu){
        return MenuApi.createMenu(getTokenValue(), menu);
    }


    /**
     * 查询菜单
     *
     * @return {@code MenuQuery }
     * @author booty
     *
     */
    public ResponseMenuQuery queryMenu(){
        return MenuApi.queryMenu(getTokenValue());
    }


    /**
     * 删除菜单
     *
     * @return {@code ResponseBase }
     * @author booty
     *
     */
    public ResponseBase deleteMenu(){
        return MenuApi.deleteMenu(getTokenValue());
    }



    /**
     * 获取设置的行业信息
     *
     * @return {@code Industry }
     * @author booty
     *
     */
    public ResponseIndustry getIndustry(){
        return MessageApi.getIndustry(getTokenValue());
    }








}
