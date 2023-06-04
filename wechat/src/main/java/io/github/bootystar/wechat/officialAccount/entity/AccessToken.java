package io.github.bootystar.wechat.officialAccount.entity;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 微信accessToken
 * @author booty
 * @date 2023/6/4 10:25
 */

public class AccessToken {

    private String access_token;
    private Long expires_in;
    private LocalDateTime expiresTime;
    public final static String REQUEST_URL = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=APPID&secret=SECRET";
    private final static Map<String,AccessToken> TOKEN_MAP=new ConcurrentHashMap<>();

    public static AccessToken getAccessToken(String appId,String appSecret){
        AccessToken accessToken = TOKEN_MAP.get(appId);
        if (accessToken!=null) {

        }
        return null;
    }

    private static AccessToken requireNewToken(String appId,String appSecret){
        String link = REQUEST_URL.replace("APPID", appId).replace("SECRET", appSecret);
        return null;
    }


    private AccessToken() {
    }
    public String getAccess_token() {
        return access_token;
    }
    public LocalDateTime getExpiresTime() {
        return expiresTime;
    }
}
