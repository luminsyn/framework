package io.github.bootystar.mybatisplus.core.param.unmodifiable;

import io.github.bootystar.mybatisplus.core.param.base.ISqlTree;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

/**
 * @author bootystar
 */
@Getter
@AllArgsConstructor
public class TreeU implements ISqlTree {

    protected List<ConditionU> conditions;

    protected TreeU child;

    protected List<SortU> sorts;

}
