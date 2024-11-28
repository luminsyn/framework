package io.github.bootystar.mybatisplus.core.param;

/**
 * 不可变条件
 * @author bootystar
 */
public class UnmodifiableCondition extends Condition {

    public UnmodifiableCondition(String connector, String field, String operator, Object value) {
        this.connector = connector;
        this.field = field;
        this.operator = operator;
        this.value = value;
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
