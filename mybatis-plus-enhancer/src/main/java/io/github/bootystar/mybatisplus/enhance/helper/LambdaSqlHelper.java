package io.github.bootystar.mybatisplus.enhance.helper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.LambdaMeta;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import org.apache.ibatis.reflection.property.PropertyNamer;

import java.util.Collection;
import java.util.List;

/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public class LambdaSqlHelper<T, V> {

    private final DynamicService<T, V> baseService;
    private final SqlHelper sqlHelper = new SqlHelper();

    protected String getFieldName(SFunction<T, ?> func) {
        return PropertyNamer.methodToProperty(LambdaUtils.extract(func).getImplMethodName());
    }

    public LambdaSqlHelper(DynamicService<T, V> baseService) {
        this.baseService = baseService;
    }

    V one() {
        return baseService.oneByDTO(sqlHelper);
    }

    List<V> list() {
        return baseService.listByDTO(sqlHelper);
    }

    IPage<V> page(Long current, Long size) {
        return baseService.pageByDTO(sqlHelper, current, size);
    }

    public <R> LambdaSqlHelper<T, V> eq(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.EQ.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> ne(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.NE.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> gt(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.GT.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> ge(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.GE.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> lt(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.LT.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> le(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.LE.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> like(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.LIKE.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> notLike(SFunction<T, R> func, R value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.NOT_LIKE.keyword, value));
        return this;
    }

    public <R> LambdaSqlHelper<T, V> in(SFunction<T, R> func, Collection<? extends R> value) {
        String fieldName = getFieldName(func);
        for (R r : value) {
            sqlHelper.addConditions(new ConditionG(fieldName, SqlKeyword.IN.keyword, r));
        }
        return this;
    }

    public <R> LambdaSqlHelper<T, V> notIn(SFunction<T, R> func, Collection<? extends R> value) {
        String fieldName = getFieldName(func);
        for (R r : value) {
            sqlHelper.addConditions(new ConditionG(fieldName, SqlKeyword.NOT_IN.keyword, r));
        }
        return this;
    }

    public LambdaSqlHelper<T, V> isNull(SFunction<T, ?> func) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.IS_NULL.keyword, null));
        return this;
    }

    public LambdaSqlHelper<T, V> isNotNull(SFunction<T, ?> func) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.IS_NOT_NULL.keyword, null));
        return this;
    }

    public LambdaSqlHelper<T, V> with(ISqlTree sqlTree) {
        sqlHelper.with(sqlTree);
        return this;
    }

    public LambdaSqlHelper<T, V> withChild(ISqlTree sqlTree) {
        sqlHelper.withChild(sqlTree);
        return this;
    }

}
