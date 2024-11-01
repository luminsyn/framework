package io.github.bootystar.wechat.miniProgram;


import io.github.bootystar.wechat.core.ApiBase;
import io.github.bootystar.wechat.miniProgram.module.msg.*;
import io.github.bootystar.wechat.miniProgram.module.user.JsCode2SessionResponse;
import io.github.bootystar.wechat.miniProgram.module.user.UserApi;
import io.github.bootystar.wechat.miniProgram.module.user.UserPhoneNumberResponse;

/**
 * 小程序api
 * @author bootystar
 * 
 */
public class MiniProgramApi extends ApiBase {


    public MiniProgramApi(String appId, String appSecret) {
        super(appId, appSecret);
    }

    /**
     * 获取用户基本信息
     *
     * @param jsCode js代码
     * @return {@link JsCode2SessionResponse }
     * @author bootystar
     */
    public JsCode2SessionResponse jsCode2session(String jsCode) {
        return UserApi.jsCode2session(jsCode,getAppId(),getAppSecret());
    }

    /**
     * 获取用户电话号码
     *
     * @param code 代码
     * @return {@link UserPhoneNumberResponse }
     * @author bootystar
     */
    public UserPhoneNumberResponse getUserPhoneNumber(String code) {
        return UserApi.getUserPhoneNumber(code, getAccessTokenValue());
    }
    
    /**
     * 敏感文本内容检测
     *
     * @param msgSecCheckBody msg sec check body
     * @return {@link MsgSecCheckResponse }
     * @author bootystar
     */
    public MsgSecCheckResponse msgSecCheck(MsgSecCheckBody msgSecCheckBody){
        return MsgApi.msgSecCheck(msgSecCheckBody,getAccessTokenValue());
    }

    /**
     * 敏感音频/图片检测
     *
     * @param mediaCheckBody 媒体检查体
     * @return {@link MediaCheckResponse }
     * @author bootystar
     */
    public MediaCheckResponse mediaCheck(MediaCheckBody mediaCheckBody){
        return MsgApi.mediaCheck(mediaCheckBody,getAccessTokenValue());
    }
    

}
