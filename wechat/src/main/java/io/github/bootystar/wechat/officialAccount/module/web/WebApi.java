package io.github.bootystar.wechat.officialAccount.module.web;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.tool.ObjectTool;
import io.github.bootystar.wechat.core.ResponseBase;
import io.github.bootystar.wechat.core.exception.ResponseException;
import lombok.SneakyThrows;

import java.net.URLEncoder;
import java.time.LocalDateTime;

/**
 * 微信网页开发API
 * @Author booty
 * @Date 2023/6/19 16:39
 */
public class WebApi {

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
     * @param appId       应用程序id
     * @param redirectURL 重定向url
     * @return {@code String }
     * @author booty
     * @date 2023/06/19 16:47
     */
    @SneakyThrows
    public static String oAuth2UrlBase(String appId ,String redirectURL){
        String url = URL_OAUTH2_URL.replace("APPID", appId);
        url = url.replace("REDIRECT_URI", URLEncoder.encode(redirectURL, "UTF-8"));
        url = url.replace("SCOPE", SCOPE_SNSAPI_BASE);
        url = url.replace("STATE", "");
        return url;
    }


    /**
     * 获取snsapi_userinfo作用域的授权url
     *
     * @param appId       应用程序id
     * @param redirectURL 重定向url
     * @return {@code String }
     * @author booty
     * @date 2023/06/19 16:46
     */
    @SneakyThrows
    public static String oAuth2UrlUserInfo(String appId,String redirectURL){
        String url = URL_OAUTH2_URL.replace("APPID", appId);
        url = url.replace("REDIRECT_URI", URLEncoder.encode(redirectURL, "UTF-8"));
        url = url.replace("SCOPE", SCOPE_SNSAPI_USERINFO);
        url = url.replace("STATE", "");
        return url;
    }

    /**
     * 获取指定作用域的授权url
     *
     * @param appId       appId
     * @param redirectURL 重定向url
     * @param scope       范围
     * @param state       状态  重定向后会带上state参数，开发者可以填写a-zA-Z0-9的参数值，最多128字节
     * @return {@code String }
     * @author booty
     * @date 2023/06/06 16:03
     */
    @SneakyThrows
    public static String oAuth2Url(String appId ,String redirectURL,String scope,String state){
        String url = URL_OAUTH2_URL.replace("APPID", appId);
        url = url.replace("REDIRECT_URI", URLEncoder.encode(redirectURL, "UTF-8"));
        url = url.replace("SCOPE", scope);
        url = url.replace("STATE", state);
        return url;
    }


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
            throw new ResponseException(token);
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
            throw new ResponseException(token);
        }
        return token;
    }


    private final static String GET_USER_INFO_URL="https://api.weixin.qq.com/sns/userinfo?access_token=ACCESS_TOKEN&openid=OPENID&lang=zh_CN";

    /**
     * 根据openId获取用户信息
     *
     * @param token  用户个人令牌
     * @param openId 开放id
     * @return {@code UserInfo }
     * @author booty
     * @date 2023/06/08 10:44
     */
    public static UserInfo getUserInfoByOpenId(String token,String openId){
        String url = GET_USER_INFO_URL.replace("ACCESS_TOKEN", token).replace("OPENID", openId);
         /*
        若出现乱码问题，不再使用HttpTool，启用此行代码
            try {
                url1 = new URL(url);
                urlConnection = (HttpURLConnection)url1.openConnection();
                try (InputStream inputStream = urlConnection.getInputStream();
                     InputStreamReader inputStreamReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
                     BufferedReader in = new BufferedReader(inputStreamReader);
                ){
                    String result = in.readLine();
                    urlConnection.disconnect();
                    UserInfo userInfo = JSON.parseObject(result, UserInfo.class);
                    UserInfo userInfo = JSON.parseObject(result, UserInfo.class);
                    if (ObjectTool.isEmpty(userInfo.getOpenid())){
                        throw new WechatResponseException(userInfo);
                    }
                    return userInfo;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
         */
        String result = HttpTool.doGet(url);
        /*
        {
          "openid": "OPENID",
          "nickname": NICKNAME,
          "sex": 1,
          "province":"PROVINCE",
          "city":"CITY",
          "country":"COUNTRY",
          "headimgurl":"https://thirdwx.qlogo.cn/mmopen/g3MonUZtNHkdmzicIlibx6iaFqAc56vxLSUfpb6n5WKSYVY0ChQKkiaJSgQ1dZuTOgvLLrhJbERQQ4eMsv84eavHiaiceqxibJxCfHe/46",
          "privilege":[ "PRIVILEGE1" "PRIVILEGE2"     ],
          "unionid": "o6_bmasdasdsad6_2sgVt7hMZOPfL"
        }
         */
        UserInfo userInfo = JSON.parseObject(result, UserInfo.class);
        if (ObjectTool.isEmpty(userInfo.getOpenid())){
            throw new ResponseException(userInfo);
        }
        return userInfo;
    }


}
