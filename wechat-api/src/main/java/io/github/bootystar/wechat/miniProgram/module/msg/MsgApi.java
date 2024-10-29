package io.github.bootystar.wechat.miniProgram.module.msg;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.wechat.tool.HttpTool;

/**
 * @author booty
 */
public class MsgApi {
    private MsgApi(){}
    
    private static final String POST_MSG_SEC_CHECK_URL="https://api.weixin.qq.com/wxa/msg_sec_check?access_token=ACCESS_TOKEN";

    /**
     * 检查输入的关键字
     *
     * @param msgSecCheckBody msg sec check body
     * @param accessToken     访问令牌
     * @return {@link MsgSecCheckResponse }
     * @author booty
     */
    public static MsgSecCheckResponse msgSecCheck(MsgSecCheckBody msgSecCheckBody,String accessToken){
        String url=POST_MSG_SEC_CHECK_URL.replace("ACCESS_TOKEN", accessToken);
        String body = JSON.toJSONString(msgSecCheckBody);
        String result = HttpTool.doPostJson(url, body);
        MsgSecCheckResponse response = JSON.parseObject(result, MsgSecCheckResponse.class);
        response.check();
        return response;
    }

    private static final String POST_MEDIA_SEC_CHECK_URL ="https://api.weixin.qq.com/wxa/media_check_async?access_token=ACCESS_TOKEN";

    /**
     * 敏感音频/图片检测
     *
     * @param mediaCheckBody 检查体  
     * @param accessToken    访问令牌
     * @return {@link MediaCheckResponse }
     * @author booty
     */
    public static MediaCheckResponse mediaCheck(MediaCheckBody mediaCheckBody,String accessToken){
        String url= POST_MEDIA_SEC_CHECK_URL.replace("ACCESS_TOKEN", accessToken);
        String body = JSON.toJSONString(mediaCheckBody);
        String result = HttpTool.doPostJson(url, body);
        MediaCheckResponse response = JSON.parseObject(result, MediaCheckResponse.class);
        response.check();
        return response;
    }
}
