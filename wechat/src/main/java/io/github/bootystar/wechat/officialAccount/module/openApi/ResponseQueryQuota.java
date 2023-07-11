package io.github.bootystar.wechat.officialAccount.module.openApi;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.wechat.core.exception.ResponseException;
import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;

/**
 * 接口调用配额查询
 * @Author booty
 * @Date 2023/6/8 14:48
 */
@Data
public class ResponseQueryQuota extends ResponseBase {
    /**
     * quota结构体
     */
    private Quota quota;


    /**
     * 每日调用接口次数
     */
    @Data
    public static class Quota{
        /**
         * 当天该账号可调用该接口的次数
         */
        private Integer daily_limit;
        /**
         * 当天已经调用的次数
         */
        private Integer used;
        /**
         * 当天剩余调用次数
         */
        private Integer remain;
    }


}
