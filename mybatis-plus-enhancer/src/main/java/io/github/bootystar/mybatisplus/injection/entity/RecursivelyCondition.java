package io.github.bootystar.mybatisplus.injection.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * 递归条件
 *
 * @author bootystar
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RecursivelyCondition {

    /**
     * 条件
     */
    protected List<Condition> conditions;

    /**
     * 子条件
     * (满足父条件后的值才会筛选子条件)
     */
    protected RecursivelyCondition child;



    public static class ImmutableRecursivelyCondition extends RecursivelyCondition {
        public ImmutableRecursivelyCondition(List<Condition> conditions, RecursivelyCondition child) {
            super(conditions, child);
        }

        public ImmutableRecursivelyCondition() {
            throw new UnsupportedOperationException("not support empty constructor");
        }

        @Override
        public void setConditions(List<Condition> conditions) {
            throw new UnsupportedOperationException("not support set value");
        }

        @Override
        public void setChild(RecursivelyCondition child) {
            throw new UnsupportedOperationException("not support set value");
        }
    }

}
