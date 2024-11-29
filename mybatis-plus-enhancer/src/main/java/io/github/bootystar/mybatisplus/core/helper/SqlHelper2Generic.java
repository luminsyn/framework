package io.github.bootystar.mybatisplus.core.helper;

import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.core.param.ParamMappingException;
import io.github.bootystar.mybatisplus.core.param.base.ISqlCondition;
import io.github.bootystar.mybatisplus.core.param.base.ISqlSort;
import io.github.bootystar.mybatisplus.core.param.base.ISqlTree;
import io.github.bootystar.mybatisplus.core.param.unmodifiable.ConditionU;
import io.github.bootystar.mybatisplus.core.param.unmodifiable.SortU;
import io.github.bootystar.mybatisplus.core.param.unmodifiable.TreeU;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public abstract class SqlHelper2Generic<T> extends TreeU {

    /**
     * 实体类
     */
    @Getter
    protected final Class<T> entityClass;

    /**
     * 属性和字段映射
     */
    protected final Map<String, String> field2JdbcColumnMap;

    public SqlHelper2Generic(ISqlTree sqlTree, Class<T> entityClass) {
        super(null, null, null);
        if (entityClass == null || sqlTree == null) {
            throw new ParamMappingException("tree or entityClass class can not be null, please check your configuration");
        }
        this.entityClass = entityClass;
        Map<String, String> field2JdbcColumnMap = MybatisPlusReflectHelper.field2JdbcColumnMap(entityClass);
        if (field2JdbcColumnMap.isEmpty()) {
            throw new ParamMappingException("entityClass %s has no field to convert, please check your configuration", entityClass.getName());
        }
        this.field2JdbcColumnMap = field2JdbcColumnMap;
        // 不在迭代时直接赋值, 保留扩展空间
        TreeU tree = recursionTree(sqlTree);
        this.conditions = tree.getConditions();
        this.child = tree.getChild();
        this.sorts = tree.getSorts();
    }

    protected TreeU recursionTree(ISqlTree sqlTree) {
        if (sqlTree == null) {
            return null;
        }
        List<? extends ISqlCondition> conditions = sqlTree.getConditions();
        List<ConditionU> validatedUnmodifiableConditions = validatedConditions(conditions);
        ISqlTree child = sqlTree.getChild();
        if (validatedUnmodifiableConditions == null || validatedUnmodifiableConditions.isEmpty()) {
            if (child != null) {
                throw new ParamMappingException("validatedConditions is null or empty, but child is not null, currentNode : %s", sqlTree);
            }
            return null;
        }
        TreeU newChild = recursionTree(child);
        return new TreeU(validatedUnmodifiableConditions, newChild, validatedSorts(sqlTree.getSorts()));
    }

    protected List<SortU> validatedSorts(List<? extends ISqlSort> sorts) {
        if (sorts == null || sorts.isEmpty()) {
            return null;
        }
        ArrayList<SortU> validatedSorts = new ArrayList<>();
        Iterator<? extends ISqlSort> sit = sorts.iterator();
        while (sit.hasNext()) {
            ISqlSort sortO = sit.next();
            String field = sortO.getField();
            Boolean desc = sortO.getDesc();
            if (desc == null) {
                desc = false;
            }
            if (field == null || field.isEmpty()) {
                log.warn("sort field [{}] is null , it will be removed", field);
                sit.remove();
                continue;
            }
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                log.warn("sort field [{}] not exist in fieldMap , it will be removed", field);
                sit.remove();
            }
            validatedSorts.add(new SortU(jdbcColumn, desc));
        }
        return Collections.unmodifiableList(validatedSorts);
    }

    public Optional<ConditionU> wrap2JdbcColumnCondition(ISqlCondition conditionO) {
        String connector = SqlKeyword.replaceConnector(conditionO.getConnector());
        String operator = SqlKeyword.replaceOperator(conditionO.getOperator());
        String field = conditionO.getField();
        Object value = conditionO.getValue();
        return wrap2JdbcColumnCondition(connector, field, operator, value);
    }

    public Optional<ConditionU> wrap2JdbcColumnCondition(String connector, String field, String operator, Object value) {
        if (field == null || field.isEmpty()) {
            return Optional.empty();
        }
        String jdbcColumn = field2JdbcColumnMap.get(field);
        if (jdbcColumn == null) {
            log.warn("condition field [{}] not exist in fieldMap , it will be removed", field);
            return Optional.empty();
        }
        connector = SqlKeyword.replaceConnector(connector);
        operator = SqlKeyword.replaceOperator(operator);
        if (!SqlKeyword.isNoneArgOperator(operator) && value == null) {
            log.warn("condition field [{}] requires value but value is null, it will be removed", field);
            return Optional.empty();
        }
        if (SqlKeyword.isMultiArgOperator(operator)) {
            if (value instanceof Iterable) {
                Iterable<?> iterable = (Iterable<?>) value;
                if (!iterable.iterator().hasNext()) {
                    log.warn("condition field [{}] requires collection but value is empty, it will be removed", field);
                    return Optional.empty();
                }
                ArrayList<Object> newContainer = new ArrayList<>();
                iterable.forEach(newContainer::add);
                value = newContainer;
            } else {
                log.warn("condition field [{}] requires collection but value is not iterable, it will be removed", field);
                return Optional.empty();
            }
        }
        if (SqlKeyword.isLikeOperator(operator)) {
            value = "%" + value + "%";
        }
        return Optional.of(new ConditionU(connector, jdbcColumn, operator, value));
    }

    protected abstract List<ConditionU> validatedConditions(List<? extends ISqlCondition> conditions);

}
