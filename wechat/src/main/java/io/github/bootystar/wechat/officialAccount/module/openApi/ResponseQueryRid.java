package io.github.bootystar.wechat.officialAccount.module.openApi;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.wechat.core.exception.ResponseException;
import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;

/**
 * 查询rid信息
 * @Author booty
 * @Date 2023/6/8 17:14
 */
@Data
public class ResponseQueryRid extends ResponseBase {
    /**
     * rid对应的请求详情
     */
    private ApiRequest request;

    /**
     * qpi请求详情
     */
    @Data
    public static class ApiRequest{
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
