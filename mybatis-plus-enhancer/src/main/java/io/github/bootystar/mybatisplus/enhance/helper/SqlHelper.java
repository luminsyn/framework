package io.github.bootystar.mybatisplus.enhance.helper;

import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import io.github.bootystar.mybatisplus.enhance.query.general.SortG;
import io.github.bootystar.mybatisplus.enhance.query.general.TreeG;
import io.github.bootystar.mybatisplus.util.ReflectHelper;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * sql助手
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends TreeG {
    {
        this.conditions = new LinkedHashSet<>(4);
        this.sorts = new LinkedHashSet<>(4);
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param condition 条件
     * @return {@link SqlHelper<T> }
     * @author bootystar
     */
    public SqlHelper<T> condition(ISqlCondition condition) {
        if (condition == null) {
            return this;
        }
        this.getConditions().add(ConditionG.of(condition));
        return this;
    }

    /**
     * 添加排序
     *
     * @param sort 排序
     * @return {@link SqlHelper<T> }
     * @author bootystar
     */
    public SqlHelper<T> sort(ISqlSort sort) {
        if (sort == null) {
            return this;
        }
        this.getSorts().add(SortG.of(sort));
        return this;
    }

    /**
     * 将指定的sql树作为条件添加
     *
     * @param sqlTree sql树
     * @return {@link SqlHelper<T> }
     * @author bootystar
     */
    public SqlHelper<T> with(ISqlTree sqlTree) {
        if (sqlTree == null || sqlTree.getConditions() == null || sqlTree.getConditions().isEmpty()) {
            return this;
        }
        LinkedHashSet<ConditionG> conditions1 = this.getConditions();
        if (conditions1 == null) {
            conditions1 = new LinkedHashSet<>();
        }
        conditions1.addAll(SqlHelper.of(sqlTree).getConditions());
        if (sqlTree.getChild() != null) {
            return this.withChild(sqlTree.getChild());
        }
        return this;
    }

    /**
     * 将指定的sql树条件作为子条件添加
     *
     * @param sqlTree sql树
     * @return {@link SqlHelper<T> }
     * @author bootystar
     */
    public SqlHelper<T> withChild(ISqlTree sqlTree) {
        TreeG tree = this;
        while (tree.getChild() != null) {
            tree = tree.getChild();
        }
        tree.setChild(SqlHelper.of(sqlTree));
        return this;
    }

    /**
     * 根据指定对象字段映射等于条件
     *
     * @param s s
     * @return {@link SqlHelper<T> }
     * @author bootystar
     */
    @SuppressWarnings("unchecked")
    public static <T> SqlHelper<T> of(T s) {
        if (s == null) {
            return new SqlHelper<>();
        }
        if (s instanceof SqlHelper<?>) {
            return (SqlHelper<T>) s;
        }
        if (s instanceof ISqlTree) {
            return ofSqlTree((ISqlTree) s, true);
        }
        SqlHelper<T> helper = new SqlHelper<>();
        if (s instanceof ISqlCondition) {
            helper.condition((ISqlCondition) s);
        }
        if (s instanceof ISqlSort) {
            helper.sort((ISqlSort) s);
        }
        Map<?, ?> map = ReflectHelper.objectToMap(s);
        for (Map.Entry<?, ?> next : map.entrySet()) {
            Object key = next.getKey();
            Object value = next.getValue();
            ConditionG condition = new ConditionG(key.toString(), value);
            helper.condition(condition);
        }
        return helper;
    }

    /**
     * 根据SqlTree生成helper
     *
     * @param tree      树
     * @param copySorts 复制排序
     * @return {@link SqlHelper<T> }
     * @author bootystar
     */
    protected static <T> SqlHelper<T> ofSqlTree(ISqlTree tree, boolean copySorts) {
        if (tree == null) {
            return new SqlHelper<>();
        }
        SqlHelper<T> helper = new SqlHelper<>();
        Collection<? extends ISqlCondition> conditions1 = tree.getConditions();
        if (conditions1 != null && !conditions1.isEmpty()) {
            helper.getConditions().addAll(conditions1.stream().map(ConditionG::of).collect(Collectors.toList()));
        }
        if (copySorts) {
            Collection<? extends ISqlSort> treeSorts = tree.getSorts();
            if (treeSorts != null && !treeSorts.isEmpty()) {
                helper.getSorts().addAll(treeSorts.stream().map(SortG::of).collect(Collectors.toList()));
            }
        }
        ISqlTree child = tree.getChild();
        if (child != null) {
            helper.setChild(ofSqlTree(child, false));
        }
        return helper;
    }

}
