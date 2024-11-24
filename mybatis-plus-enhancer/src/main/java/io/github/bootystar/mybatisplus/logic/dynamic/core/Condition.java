package io.github.bootystar.mybatisplus.logic.dynamic.core;

import io.github.bootystar.mybatisplus.logic.dynamic.enums.SqlKeyword;
import lombok.*;

/**
 * 条件参数
 *
 * @author bootystar
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Condition {

    /**
     * 连接符
     * 和上一个条件的关系(AND 或 OR)
     * 默认AND
     * 递归
     */
    protected String connector = SqlKeyword.AND.keyword;

    /**
     * 字段
     */
    protected String field;

    /**
     * 运算符(=,>,<,!=,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN)
     * 默认=
     */
    protected String operator = SqlKeyword.EQ.keyword;

    /**
     * 值
     */
    protected Object value;

}
