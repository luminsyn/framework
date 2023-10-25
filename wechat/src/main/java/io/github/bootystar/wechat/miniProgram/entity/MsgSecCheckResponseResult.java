package io.github.bootystar.wechat.miniProgram.entity;

import lombok.Data;

/**
 * 综合结果
 * @author booty
 * @since 2023/6/15 11:03
 */
@Data
public class MsgSecCheckResponseResult {
    /**
     * 建议，有risky、pass、review三种值
     */
    private String suggest;
    /**
     * 命中标签枚举值，100 正常；10001 广告；20001 时政；20002 色情；20003 辱骂；20006 违法犯罪；20008 欺诈；20012 低俗；20013 版权；21000 其他
     */
    private Integer label;
}
