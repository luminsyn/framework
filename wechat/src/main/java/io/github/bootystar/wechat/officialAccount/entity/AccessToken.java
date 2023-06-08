package io.github.bootystar.wechat.officialAccount.entity;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.tool.ObjectTool;
import io.github.bootystar.wechat.common.exception.WechatResponseException;
import io.github.bootystar.wechat.entity.ResponseBase;
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

        // 设置过期时间
        token.setExpiresTime(LocalDateTime.now().plusSeconds(token.getExpires_in()));
        if (ObjectTool.isEmpty(token.getAccess_token())){
//            String msg=String.format("wechat accessToken required failed,response code:%s, responseMsg:%s",token.getErrcode(),token.getErrmsg());
            throw new WechatResponseException(token);
        }
        return token;
    }





}
