package io.github.bootystar.wechat.miniProgram.module.user;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.wechat.tool.HttpTool;

/**
 * @author booty
 */
public class UserApi {

    private static final String GET_JS_CODE_2_SESSION ="https://api.weixin.qq.com/sns/jscode2session?appid=APPID&secret=SECRET&js_code=JSCODE&grant_type=authorization_code";

    /**
     * 获取用户信息
     *
     * @param jsCode    js代码
     * @param appId     应用程序id
     * @param appSecret 应用程序秘密
     * @return {@link JsCode2SessionResponse }
     * @author booty
     */
    public static JsCode2SessionResponse jsCode2session(String jsCode,String appId,String appSecret) {
        String url = GET_JS_CODE_2_SESSION
                .replace("JSCODE", jsCode)
                .replace("APPID", appId)
                .replace("SECRET", appSecret)
                ;
        String result = HttpTool.doGet(url);
        JsCode2SessionResponse response = JSON.parseObject(result, JsCode2SessionResponse.class);
        response.check();
        return response;
    }
    
    private static final String POST_GET_USER_PHONE_NUMBER ="https://api.weixin.qq.com/wxa/business/getuserphonenumber?access_token=ACCESS_TOKEN";

    /**
     * 获取用户手机号
     *
     * @param code        代码
     * @param accessToken 访问令牌
     * @return {@link UserPhoneNumberResponse }
     * @author booty
     */
    public static UserPhoneNumberResponse getUserPhoneNumber(String code ,String accessToken) {
        String url = POST_GET_USER_PHONE_NUMBER.replace("ACCESS_TOKEN", accessToken);
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("code",code);
        String result = HttpTool.doPostJson(url, jsonObject.toJSONString());
        UserPhoneNumberResponse response = JSON.parseObject(result, UserPhoneNumberResponse.class);
        response.check();
        return response;
    }
    
    
    
    
}
