package io.github.bootystar.mybatisplus.core.param.normal;

import io.github.bootystar.mybatisplus.core.param.base.ISqlTree;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * 条件树
 *
 * @author bootystar
 */
@Setter
@Getter
public class TreeN implements ISqlTree {

    /**
     * 条件
     */
    protected LinkedHashSet<ConditionN> conditions;

    /**
     * 子条件
     * (满足父条件后的值才会筛选子条件)
     */
    protected TreeN child;

    /**
     * 排序条件列表
     */
    protected LinkedHashSet<SortN> sorts;

}
