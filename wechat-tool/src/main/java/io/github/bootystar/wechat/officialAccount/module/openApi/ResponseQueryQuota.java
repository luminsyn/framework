package io.github.bootystar.wechat.officialAccount.module.openApi;

import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;

/**
 * 接口调用配额查询
 * @author bootystar
 * 
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
