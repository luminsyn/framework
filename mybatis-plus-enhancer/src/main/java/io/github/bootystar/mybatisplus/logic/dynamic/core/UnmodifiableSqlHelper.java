package io.github.bootystar.mybatisplus.logic.dynamic.core;

import io.github.bootystar.mybatisplus.logic.dynamic.ConditionConvertException;
import io.github.bootystar.mybatisplus.logic.dynamic.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.util.ReflectHelper4MybatisPlus;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.javassist.compiler.ast.Variable;

import java.util.*;

/**
 * 不可变SQL拼接器
 *
 * @author bootystar
 */
@Getter
@Slf4j
public class UnmodifiableSqlHelper<T> extends ConditionTree {

    /**
     * 实体类
     */
    private final Class<T> entityClass;

    /**
     * 排序条件列表
     */
    protected final List<? extends Sort> sorts;

    public UnmodifiableSqlHelper(SqlHelper baseHelper, Class<T> entityClass) {
        if (entityClass == null || baseHelper == null) {
            throw new ConditionConvertException("baseHelper or entityClass class can not be null, please check your configuration");
        }
        Map<String, String> map = ReflectHelper4MybatisPlus.dynamicFieldsMap(entityClass);
        if (map.isEmpty()) {
            throw new ConditionConvertException("entityClass %s has no field to convert, please check your configuration", entityClass.getName());
        }
        this.entityClass = entityClass;
        this.sorts = validatedUnmodifiableSorts(baseHelper.getSorts(), map);
        UnmodifiableConditionTree validatededUnmodifiableConditionTree = validatedUnmodifiableConditionTree(baseHelper, map);
        if (validatededUnmodifiableConditionTree != null) {
            this.conditions = validatededUnmodifiableConditionTree.getConditions();
            this.child = validatededUnmodifiableConditionTree.getChild();
        }
    }

    public static UnmodifiableConditionTree validatedUnmodifiableConditionTree(ConditionTree conditionTree, Map<String, String> map) {
        if (conditionTree == null) {
            return null;
        }
        List<? extends Condition> conditions = conditionTree.getConditions();
        List<UnmodifiableCondition> validatedConditions = validatedUnmodifiableConditions(conditions, map);
        ConditionTree child = conditionTree.getChild();
        if (validatedConditions == null || validatedConditions.isEmpty()) {
            if (child != null) {
                throw new ConditionConvertException("validatedConditions is null or empty, but child is not null, currentNode : %s", conditionTree);
            }
            return null;
        }
        UnmodifiableConditionTree newChild = validatedUnmodifiableConditionTree(child, map);
        return new UnmodifiableConditionTree(validatedConditions, newChild);
    }

    protected static List<UnmodifiableCondition> validatedUnmodifiableConditions(List<? extends Condition> conditions, Map<String, String> map) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<UnmodifiableCondition> validatedConditions = new ArrayList<>();
        for (Condition conditionO : conditions) {
            String connector = SqlKeyword.replaceConnector(conditionO.getConnector());
            String operator = SqlKeyword.replaceOperator(conditionO.getOperator());
            String field = conditionO.getField();
            Object value = conditionO.getValue();
            if (field == null || field.isEmpty()) {
                continue;
            }
            String jdbcColumn = map.get(field);
            if (jdbcColumn == null) {
                log.debug("condition field [{}] not exist in fieldMap, it will be removed", field);
                continue;
            }
            if (!SqlKeyword.isNoneArgOperator(operator) && value == null) {
                log.debug("condition field [{}] requires value but value is null, it will be removed", field);
                continue;
            }
            if (SqlKeyword.isMultiArgOperator(operator)) {
                if (value instanceof Iterable) {
                    Iterable<?> iterable = (Iterable<?>) value;
                    if (!iterable.iterator().hasNext()) {
                        log.debug("condition field [{}] requires collection but value is empty, it will be removed", field);
                        continue;
                    }
                    ArrayList<Object> newContainer = new ArrayList<>();
                    iterable.forEach(newContainer::add);
                    value = newContainer;
                } else {
                    log.debug("condition field [{}] requires collection but value is not iterable, it will be removed", field);
                    continue;
                }
            }
            if (SqlKeyword.isLikeOperator(operator)) {
                value = "%" + value + "%";
            }
            validatedConditions.add(new UnmodifiableCondition(connector, jdbcColumn, operator, value));
        }
        return Collections.unmodifiableList(validatedConditions);
    }

    private static List<Sort> validatedUnmodifiableSorts(List<? extends Sort> sorts, Map<String, String> map) {
        if (sorts == null || sorts.isEmpty()) {
            return null;
        }
        ArrayList<Sort> validatedSorts = new ArrayList<>();
        Iterator<? extends Sort> sit = sorts.iterator();
        while (sit.hasNext()) {
            Sort sortO = sit.next();
            String field = sortO.getField();
            Boolean desc = sortO.getDesc();
            if (desc == null) {
                desc = false;
            }
            if (field == null || field.isEmpty()) {
                log.debug("sort field [{}] is null , it will be removed", field);
                sit.remove();
                continue;
            }
            String jdbcColumn = map.get(field);
            if (jdbcColumn == null) {
                log.debug("sort field [{}] not exist in fieldMap , it will be removed", field);
                sit.remove();
            }
            validatedSorts.add(new UnmodifiableSort(jdbcColumn, desc));
        }
        return Collections.unmodifiableList(validatedSorts);
    }

}
