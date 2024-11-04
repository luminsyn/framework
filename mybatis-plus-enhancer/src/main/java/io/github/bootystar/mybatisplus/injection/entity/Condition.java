package io.github.bootystar.mybatisplus.injection.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 条件
 * @author bootystar
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Condition {

    /**
     * 连接符
     * 和上一个条件的关系(AND 或 OR)
     * 默认AND
     */
    private String connector = "AND";

    /**
     * 字段
     */
    private String field;
    
    /**
     * 运算符(=,>,<,!=,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN)
     * 默认=
     */
    private String operator = "=";

    /**
     * 值
     */
    private Object value;


    public void setOperator(String operator) {
        if (operator == null || operator.isEmpty()) {
            this.operator = "=";
            return;
        }
        this.operator = operator.toUpperCase();
    }

    public void setConnector(String connector) {
        if (connector == null || connector.isEmpty()) {
            this.connector = "AND";
            return;
        }
        this.connector = connector.toUpperCase();
    }

    public Condition newInstance(){
        Condition condition = new Condition();
        condition.setConnector(connector);
        condition.setField(field);
        condition.setOperator(operator);
        condition.setValue(value);
        return condition;
    }
}
