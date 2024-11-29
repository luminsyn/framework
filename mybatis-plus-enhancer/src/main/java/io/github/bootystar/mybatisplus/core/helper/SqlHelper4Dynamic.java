package io.github.bootystar.mybatisplus.core.helper;

import io.github.bootystar.mybatisplus.core.param.base.ISqlCondition;
import io.github.bootystar.mybatisplus.core.param.base.ISqlTree;
import io.github.bootystar.mybatisplus.core.param.unmodifiable.ConditionU;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Getter
@Slf4j
public class SqlHelper4Dynamic<T> extends SqlHelper2Generic<T> {

    public SqlHelper4Dynamic(ISqlTree sourceTree, Class<T> entityClass) {
        super(sourceTree, entityClass);
    }

    public static <T> SqlHelper4Dynamic<T> of(ISqlTree conditionTrees, Class<T> entityClass) {
        return new SqlHelper4Dynamic<>(conditionTrees, entityClass);
    }

    @Override
    protected Collection<ConditionU> validatedConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<ConditionU> result = new ArrayList<>(conditions.size());
        for (ISqlCondition conditionO : conditions) {
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        return result;
    }

}
