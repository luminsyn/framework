package io.github.bootystar.mybatisplus.core.param.unmodifiable;

import io.github.bootystar.mybatisplus.core.param.base.ISqlTree;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * @author bootystar
 */
@Getter
public class TreeU implements ISqlTree {

    protected Collection<ConditionU> conditions;

    protected TreeU child;

    protected Collection<SortU> sorts;

    public TreeU(Collection<ConditionU> conditions, Collection<SortU> sorts, TreeU child) {
        this.conditions = conditions == null ? null : Collections.unmodifiableCollection(new LinkedHashSet<>(conditions));
        this.sorts = sorts == null ? null : Collections.unmodifiableCollection(new LinkedHashSet<>(sorts));
        this.child = child;
    }
}
