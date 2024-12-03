package io.github.bootystar.mybatisplus.enhance.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.ConditionU;
import lombok.Getter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;

/**
 * @author bootystar
 */
@Getter
public class DynamicSqlSqlHelper<T> extends UnmodifiableSqlHelper<T> {

    public DynamicSqlSqlHelper(ISqlTree sourceTree, Class<T> entityClass) {
        super(sourceTree, entityClass);
        initProperties(sourceTree);
    }

    @Override
    protected Collection<ConditionU> validatedConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<ConditionU> result = new ArrayList<>(conditions.size());
        HashMap<String, Object> illegalConditionMap = new HashMap<>();
        for (ISqlCondition conditionO : conditions) {
            String field = conditionO.getField();
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                illegalConditionMap.put(field, conditionO.getValue());
                continue;
            }
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        if (!illegalConditionMap.isEmpty()) {
            this.paramMap = Collections.unmodifiableMap(illegalConditionMap);
        }
        return result;
    }

}
