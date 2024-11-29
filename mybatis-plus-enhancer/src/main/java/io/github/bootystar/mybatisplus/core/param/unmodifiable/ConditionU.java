package io.github.bootystar.mybatisplus.core.param.unmodifiable;

import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.core.param.base.ISqlCondition;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 不可变条件
 *
 * @author bootystar
 */
@Getter
@AllArgsConstructor
public class ConditionU implements ISqlCondition {

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
     */
    protected String operator = SqlKeyword.EQ.keyword;

    /**
     * 值(若是多个值,如in ,则value为集合)
     */
    protected Object value;

}
