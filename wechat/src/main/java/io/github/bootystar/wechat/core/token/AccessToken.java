package io.github.bootystar.wechat.core.token;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.tool.ObjectTool;
import io.github.bootystar.wechat.core.ResponseBase;
import io.github.bootystar.wechat.core.exception.ResponseException;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * 微信accessToken
 * @author booty
 * @date 2023/6/4 10:25
 */
@Data
public class AccessToken extends ResponseBase {
    /**
     *获取到的凭证
     */
    private String access_token;
    /**
     * 凭证有效时间，单位：秒
     */
    private Long expires_in;

    // =============================

    private LocalDateTime expiresTime;
    private final static String ACCESS_TOKEN_REQUEST_URL = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=APPID&secret=SECRET";


    /**
     * 获取访问令牌
     * access_token是公众号的全局唯一接口调用凭据，公众号调用各接口时都需使用access_token。
     * 开发者需要进行妥善保存。
     * access_token的存储至少要保留512个字符空间。
     * access_token的有效期目前为2个小时，需定时刷新，重复获取将导致上次获取的access_token失效。
     *
     * @param appId     应用程序id
     * @param appSecret 应用程序秘密
     * @return {@code AccessToken }
     * @author booty
     * @date 2023/06/06 15:30
     */
    public static AccessToken createAccessToken(String appId, String appSecret){
        String url = ACCESS_TOKEN_REQUEST_URL.replace("APPID", appId).replace("SECRET", appSecret);
        String result = HttpTool.doGet(url);
        AccessToken token = JSON.parseObject(result, AccessToken.class);
        /*
        {"access_token":"ACCESS_TOKEN","expires_in":7200}
         */
        if (ObjectTool.isEmpty(token.getAccess_token())){
            throw new ResponseException(token);
        }
        // 设置过期时间
        token.setExpiresTime(LocalDateTime.now().plusSeconds(token.getExpires_in()));
        return token;
    }


    private final static String STABLE_TOKEN_REQUEST_URL="https://api.weixin.qq.com/cgi-bin/stable_token";


    /**
     * 创造稳定访问令牌
     * 功能描述
     * 获取公众号全局后台接口调用凭据，有效期最长为7200s，开发者需要进行妥善保存；
     * 有两种调用模式: 1. 普通模式，access_token 有效期内重复调用该接口不会更新 access_token，绝大部分场景下使用该模式；2. 强制刷新模式，会导致上次获取的 access_token 失效，并返回新的 access_token；
     * 该接口调用频率限制为 1万次 每分钟，每天限制调用 50w 次；
     * 与获取Access token获取的调用凭证完全隔离，互不影响。该接口仅支持 POST JSON 形式的调用；
     *
     * @param appId     应用程序id
     * @param appSecret 应用程序秘密
     * @param refresh   默认使用 false。1. force_refresh = false 时为普通调用模式，access_token 有效期内重复调用该接口不会更新 access_token；2. 当force_refresh = true 时为强制刷新模式，会导致上次获取的 access_token 失效，并返回新的 access_token
     * @return {@link AccessToken }
     * @author booty
     * @date 2023/06/11 15:11
     */
    public static AccessToken createStableAccessToken(String appId, String appSecret,boolean refresh){
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("grant_type","client_credential");
        jsonObject.put("appid",appId);
        jsonObject.put("secret",appSecret);
        jsonObject.put("force_refresh",refresh);
        String body = jsonObject.toJSONString();
        String result = HttpTool.doPostJson(STABLE_TOKEN_REQUEST_URL,body);
        AccessToken token = JSON.parseObject(result, AccessToken.class);
        /*
        {"access_token":"ACCESS_TOKEN","expires_in":7200}
         */
        if (ObjectTool.isEmpty(token.getAccess_token())){
            throw new ResponseException(token);
        }
        // 设置过期时间
        token.setExpiresTime(LocalDateTime.now().plusSeconds(token.getExpires_in()));
        return token;
    }



}
