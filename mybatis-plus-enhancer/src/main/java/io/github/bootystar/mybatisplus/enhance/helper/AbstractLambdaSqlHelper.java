package io.github.bootystar.mybatisplus.enhance.helper;

import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import io.github.bootystar.mybatisplus.enhance.query.general.SortG;
import io.github.bootystar.mybatisplus.enhance.query.general.TreeG;
import io.github.bootystar.mybatisplus.util.ReflectHelper;
import org.apache.ibatis.reflection.property.PropertyNamer;

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
public abstract class AbstractLambdaSqlHelper<T, C extends AbstractLambdaSqlHelper<T, C>> extends TreeG {
    protected boolean orNext;

    {
        this.conditions = new LinkedHashSet<>(4);
        this.sorts = new LinkedHashSet<>(4);
    }

    protected String getFieldName(SFunction<T, ?> getter) {
        return PropertyNamer.methodToProperty(LambdaUtils.extract(getter).getImplMethodName());
    }

    protected abstract C returnValue();

    /**
     * 将所有已有条件设置为子条件(仅查询条件, 不会影响排序)
     * 运行方法后,若未添加新条件会终止查询
     *
     * @return {@link C }
     * @author bootystar
     */
    public C requiredNext() {
        TreeG oldTree = this.getChild();
        TreeG newTree = new TreeG();
        this.setChild(newTree);
        newTree.setChild(oldTree);
        newTree.setConditions(this.getConditions());
        this.conditions = new LinkedHashSet<>();
        return returnValue();
    }

    /**
     * 将下一个方法引用的条件链接符号设置为or
     *
     * @return {@link C }
     * @author bootystar
     */
    public C or() {
        this.orNext = true;
        return returnValue();
    }

    protected boolean isOrNext() {
        boolean orNext1 = this.orNext;
        this.orNext = false;
        return orNext1;
    }

    /**
     * 等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C eq(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.EQ.keyword, value));
        return returnValue();
    }

    /**
     * 不等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C ne(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.NE.keyword, value));
        return returnValue();
    }

    /**
     * 大于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C gt(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.GT.keyword, value));
        return returnValue();
    }

    /**
     * 大于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C ge(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.GE.keyword, value));
        return returnValue();
    }

    /**
     * 小于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C lt(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.LT.keyword, value));
        return returnValue();
    }

    /**
     * 小于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C le(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.LE.keyword, value));
        return returnValue();
    }

    /**
     * 模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C like(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.LIKE.keyword, value));
        return returnValue();
    }

    /**
     * 不模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C notLike(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.NOT_LIKE.keyword, value));
        return returnValue();
    }

    /**
     * in查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C in(SFunction<T, R> getter, Collection<? extends R> value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.IN.keyword, value));
        return returnValue();
    }

    /**
     * notin查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public <R> C notIn(SFunction<T, R> getter, Collection<? extends R> value) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.NOT_IN.keyword, value));
        return returnValue();
    }

    /**
     * 指定字段为空
     *
     * @param getter 对象getter方法
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public C isNull(SFunction<T, ?> getter) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.IS_NULL.keyword, null));
        return returnValue();
    }

    /**
     * 指定字段不为空
     *
     * @param getter 对象getter方法
     * @return {@link AbstractLambdaSqlHelper }<{@link T }>
     * @author bootystar
     */
    public C isNotNull(SFunction<T, ?> getter) {
        this.condition(new ConditionG(this.isOrNext(), this.getFieldName(getter), SqlKeyword.IS_NOT_NULL.keyword, null));
        return returnValue();
    }

    public C orderByAsc(SFunction<T, ?> getter) {
        this.sort(new SortG(this.getFieldName(getter), false));
        return returnValue();
    }

    public C orderByDesc(SFunction<T, ?> getter) {
        this.sort(new SortG(this.getFieldName(getter), true));
        return returnValue();
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param condition 条件
     * @return {@link C }
     * @author bootystar
     */
    private C condition(ISqlCondition condition) {
        if (condition == null) {
            return returnValue();
        }
        this.getConditions().add(ConditionG.of(condition));
        return returnValue();
    }

    /**
     * 添加排序
     *
     * @param sort 排序
     * @return {@link C }
     * @author bootystar
     */
    private C sort(ISqlSort sort) {
        if (sort == null) {
            return returnValue();
        }
        this.getSorts().add(SortG.of(sort));
        return returnValue();
    }

    /**
     * 将指定的sql树作为条件添加
     *
     * @param sqlTree sql树
     * @return {@link C }
     * @author bootystar
     */
    private C with(ISqlTree sqlTree) {
        if (sqlTree == null || sqlTree.getConditions() == null || sqlTree.getConditions().isEmpty()) {
            return returnValue();
        }
        this.getConditions().addAll(SqlHelper.of(sqlTree).getConditions());
        if (sqlTree.getChild() != null) {
            return returnValue();
        }
        return returnValue();
    }

    /**
     * 将指定的sql树条件作为子条件添加
     *
     * @param sqlTree sql树
     * @return {@link C }
     * @author bootystar
     */
    private C withChild(ISqlTree sqlTree) {
        TreeG tree = this;
        while (tree.getChild() != null) {
            tree = tree.getChild();
        }
        tree.setChild(SqlHelper.of(sqlTree));
        return returnValue();
    }


}
