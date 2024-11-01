package io.github.bootystar.wechat.miniProgram.module.msg;

import lombok.Data;

/**
 * 详细检测结果
 * @author bootystar
 * 
 */
@Data
public class MsgSecCheckResponseDetail {
    /**
     * 策略类型
     */
    private String strategy;
    /**
     * 错误码，仅当该值为0时，该项结果有效
     */
    private Integer errcode;
    /**
     * 建议，有risky、pass、review三种值
     */
    private String suggest;
    /**
     * 命中标签枚举值，100 正常；10001 广告；20001 时政；20002 色情；20003 辱骂；20006 违法犯罪；20008 欺诈；20012 低俗；20013 版权；21000 其他
     */
    private Integer label;
    /**
     * 命中的自定义关键词
     */
    private String keyword;
    /**
     * 0-100，代表置信度，越高代表越有可能属于当前返回的标签（label）
     */
    private Integer prob;

}
