package io.github.bootystar.mybatisplus.logic.injection.dto;

import lombok.*;

/**
 * 条件
 * @author bootystar
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConditionDTO {

    /**
     * 连接符
     * 和上一个条件的关系(AND 或 OR)
     * 默认AND
     * 递归
     */
    protected String connector = "AND";

    /**
     * 字段
     */
    protected String field;
    
    /**
     * 运算符(=,>,<,!=,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN)
     * 默认=
     */
    protected String operator = "=";

    /**
     * 值
     */
    protected Object value;

    @Getter
    public static final class ImmutableCondition extends ConditionDTO {
        public ImmutableCondition() {
            throw new UnsupportedOperationException("not support empty constructor");
        }

        public ImmutableCondition(String connector, String field, String operator, Object value) {
            super(connector, field, operator, value);
        }

        @Override
        public void setValue(Object value) {
            throw new UnsupportedOperationException("not support set value");
        }

        @Override
        public void setField(String field) {
            throw new UnsupportedOperationException("not support set value");
        }

        @Override
        public void setOperator(String operator) {
            throw new UnsupportedOperationException("not support set value");
        }

        @Override
        public void setConnector(String connector) {
            throw new UnsupportedOperationException("not support set value");
        }
    }
}
