package io.github.bootystar.mybatisplus.injection.entity;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * 可递归的条件
 *
 * @author bootystar
 */
@Data
public class RecursivelyCondition {

    /**
     * 条件
     */
    private List<Condition> conditions;

    /**
     * 条件与子条件的关系(and 或 or)
     * 默认and
     */
    private String connector = "AND";

    /**
     * 子条件
     */
    private RecursivelyCondition children;


    public void setConnector(String connector) {
        if (connector == null || connector.isEmpty()) {
            this.connector = "AND";
            return;
        }
        this.connector = connector.toUpperCase();
    }

    /**
     * 获取拷贝的副本
     *
     * @return {@link RecursivelyCondition }
     * @author bootystar
     */
    public RecursivelyCondition newInstance() {
        RecursivelyCondition instance = new RecursivelyCondition();
        ArrayList<Condition> conditionsN = new ArrayList<>();
        if (conditions != null) {
            for (Condition condition : conditions) {
                conditionsN.add(condition.newInstance());
            }
        }
        instance.setConditions(conditionsN);
        instance.setConnector(connector);
        if (children != null){
            instance.setChildren(children.newInstance());
        }
        return instance;
    }

}
