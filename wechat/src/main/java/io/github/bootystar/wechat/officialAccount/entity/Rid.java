package io.github.bootystar.wechat.officialAccount.entity;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.wechat.common.exception.WechatResponseException;
import io.github.bootystar.wechat.entity.ResponseBase;
import lombok.Data;

/**
 * 查询rid信息
 * @Author booty
 * @Date 2023/6/8 17:14
 */
@Data
public class Rid extends ResponseBase {
    /**
     * rid对应的请求详情
     */
    private Request request;
    private static final String POST_QUERY_RID="https://api.weixin.qq.com/cgi-bin/openapi/rid/get?access_token=ACCESS_TOKEN";


    /**
     * 查询错误信息
     *
     * @param accessToken 访问令牌
     * @param rid         错误rid
     * @return {@code Rid }
     * @author booty
     * @date 2023/06/08 17:22
     */
    public static Rid queryRid(String accessToken,String rid){
        String url = POST_QUERY_RID.replace("ACCESS_TOKEN", accessToken);
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("rid",rid);
        String body = jsonObject.toJSONString();
        String result = HttpTool.doPostJson(url, body);
        /*
         {
              "errcode":0,
              "errmsg":"ok",
              "request":{
                  "invoke_time":1635156704,
                  "cost_in_ms":30,
                  "request_url":"access_token=50_Im7xxxx",
                  "request_body":"",
                  "response_body":"{\"errcode\":45009,\"errmsg\":\"reach max api daily quota limit rid: 617682e0-09059ac5-34a8e2ea\"}",
                  "client_ip": "113.xx.70.51"
              }
          }
         */
        Rid ridEntity = JSON.parseObject(result, Rid.class);
        if (ridEntity.getErrcode()!=0){
            throw new WechatResponseException(ridEntity);
        }
        return ridEntity;
    }


    /**
     * request结构体
     */
    @Data
    private static class Request{
        /**
         * 发起请求的时间戳
         */
        private Long invoke_time;
        /**
         * 请求毫秒级耗时
         */
        private Integer cost_in_ms;
        /**
         * 请求的URL参数
         */
        private String request_url;
        /**
         * post请求的请求参数
         */
        private String request_body;
        /**
         * 接口请求返回参数
         */
        private String response_body;
        /**
         * 接口请求的客户端ip
         */
        private String client_ip;
    }

}
