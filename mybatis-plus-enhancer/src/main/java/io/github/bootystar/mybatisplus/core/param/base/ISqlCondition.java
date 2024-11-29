package io.github.bootystar.mybatisplus.core.param.base;

/**
 * SQL条件
 * @author bootystar
 */
public interface ISqlCondition {

    String getConnector();

    String getField();

    String getOperator();

    Object getValue();

}
