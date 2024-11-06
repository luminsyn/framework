package io.github.bootystar.mybatisplus.base.injection.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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
public class RecursionDTO {

    /**
     * 条件
     */
    protected List<ConditionDTO> conditions;

    /**
     * 子条件
     * (满足父条件后的值才会筛选子条件)
     */
    protected RecursionDTO child;



    public static class ImmutableRecursivelyCondition extends RecursionDTO {
        public ImmutableRecursivelyCondition(List<ConditionDTO> conditions, RecursionDTO child) {
            super(conditions, child);
        }

        public ImmutableRecursivelyCondition() {
            throw new UnsupportedOperationException("not support empty constructor");
        }

        @Override
        public void setConditions(List<ConditionDTO> conditions) {
            throw new UnsupportedOperationException("not support set value");
        }

        @Override
        public void setChild(RecursionDTO child) {
            throw new UnsupportedOperationException("not support set value");
        }
    }

}
