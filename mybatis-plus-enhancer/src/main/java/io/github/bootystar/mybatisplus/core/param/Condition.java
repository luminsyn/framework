package io.github.bootystar.mybatisplus.core.param;

import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

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
     * 和上一个条件的关系(AND 或 OR),默认AND
     */
    protected String connector = SqlKeyword.AND.keyword;

    /**
     * 字段名
     */
    protected String field;

    /**
     * 运算符(=,>,<,!=,<>,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN),默认=
     *
     */
    protected String operator = SqlKeyword.EQ.keyword;

    /**
     * 值(若是多个值,如in ,则value为集合)
     */
    protected Object value;

    public Condition(String field, Object value) {
        this.field = field;
        this.value = value;
    }

    public Condition(String field, String operator, Object value) {
        this.field = field;
        this.operator = operator;
        this.value = value;
    }
}
