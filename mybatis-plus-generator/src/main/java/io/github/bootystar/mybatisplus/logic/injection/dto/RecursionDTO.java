package io.github.bootystar.mybatisplus.logic.injection.dto;

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

}
