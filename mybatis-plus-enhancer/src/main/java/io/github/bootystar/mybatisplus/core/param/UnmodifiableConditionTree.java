package io.github.bootystar.mybatisplus.core.param;

import java.util.Collections;
import java.util.List;

/**
 * @author bootystar
 */
public class UnmodifiableConditionTree extends ConditionTree {

    public UnmodifiableConditionTree(List<? extends UnmodifiableCondition> conditions, UnmodifiableConditionTree child) {
        this.conditions = Collections.unmodifiableList(conditions);
        this.child = child;
    }

    @Override
    public void setConditions(List<? extends Condition> conditions) {
        throw new UnsupportedOperationException("not support set value");
    }

    @Override
    public void setChild(ConditionTree child) {
        throw new UnsupportedOperationException("not support set value");
    }

}
