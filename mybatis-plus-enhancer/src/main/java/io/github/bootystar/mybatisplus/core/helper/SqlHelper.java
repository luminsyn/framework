package io.github.bootystar.mybatisplus.core.helper;

import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.core.param.base.ISqlCondition;
import io.github.bootystar.mybatisplus.core.param.normal.ConditionN;
import io.github.bootystar.mybatisplus.core.param.normal.SortN;
import io.github.bootystar.mybatisplus.core.param.normal.TreeN;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.SneakyThrows;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 动态sql助手
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper extends TreeN {

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     *
     * @param entity   实体
     * @param operator 操作符号
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addRequiredConditions(Object entity, String operator) {
        return addRequiredConditions(conditionsFromEntity(entity, operator));
    }

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     * 前置条件新>前置条件旧>一般条件
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addRequiredConditions(ISqlCondition... conditions) {
        return addRequiredConditions(Arrays.asList(conditions));
    }

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addRequiredConditions(List<? extends ISqlCondition> conditions) {
        TreeN oldTree = getChild();
        TreeN newTree = new TreeN();
        setChild(newTree);
        newTree.setChild(oldTree);
        newTree.setConditions(conditions.stream().map(ConditionN::of).collect(Collectors.toList()));
        return this;
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param entity   实体
     * @param operator 操作符 {@link SqlKeyword}
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addConditions(Object entity, String operator) {
        return addConditions(conditionsFromEntity(entity, operator));
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addConditions(ISqlCondition... conditions) {
        return addConditions(new ArrayList<>(Arrays.asList(conditions)));
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addConditions(List<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        List<ConditionN> conditionsO = getConditions();
        int size = conditionsO == null ? conditions.size() : conditionsO.size() + conditions.size();
        ArrayList<ConditionN> conditionsN = new ArrayList<>(size);
        conditionsN.addAll(conditions.stream().map(ConditionN::of).collect(Collectors.toList()));
        if (conditionsO != null) {
            conditionsN.addAll(conditionsO);
        }
        setConditions(conditionsN);
        return this;
    }

    public SqlHelper addSorts(SortN... sorts) {
        if (sorts == null || sorts.length == 0) {
            return this;
        }
        List<SortN> sortsO = getSorts();
        int size = sortsO == null ? sorts.length : sortsO.size() + sorts.length;
        ArrayList<SortN> sortsN = new ArrayList<>(size);
        sortsN.addAll(Arrays.asList(sorts));
        if (sortsO != null) {
            sortsN.addAll(sortsO);
        }
        setSorts(sortsN);
        return this;
    }

    /**
     * 根据实体类生成条件
     *
     * @param entity   实体类
     * @param operator SQL操作符号 参考{@link SqlKeyword }
     * @return {@link List }<{@link ConditionN }>
     * @author bootystar
     */
    @SneakyThrows
    public static List<ConditionN> conditionsFromEntity(Object entity, String operator) {
        if (entity == null) {
            throw new IllegalStateException("entity is null");
        }
        Map<String, Field> fieldMap = MybatisPlusReflectHelper.fieldMap(entity.getClass());
        List<ConditionN> conditions = new ArrayList<>();
        for (Field field : fieldMap.values()) {
            Object value = field.get(entity);
            if (value == null) continue;
            ConditionN condition = new ConditionN();
            condition.setField(field.getName());
            condition.setOperator(operator);
            condition.setValue(value);
            conditions.add(condition);
        }
        return conditions;
    }

    /**
     * 根据指定对象字段,映射等于条件
     *
     * @param s 指定对象
     * @param operator SQL操作符号 参考{@link SqlKeyword }
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public static SqlHelper of(Object s, String operator) {
        if (s == null) {
            return new SqlHelper();
        }
        if (s instanceof SqlHelper) {
            return (SqlHelper) s;
        }
        SqlHelper sqlHelper = new SqlHelper();
        if (s instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) s;
            Iterator<? extends Map.Entry<?, ?>> iterator = map.entrySet().iterator();
            List<ConditionN> conditions = new ArrayList<>();
            while (iterator.hasNext()) {
                Map.Entry<?, ?> next = iterator.next();
                Object key = next.getKey();
                Object value = next.getValue();
                ConditionN conditionN = new ConditionN(key.toString(), value);
                conditions.add(conditionN);
            }
            sqlHelper.addConditions(conditions);
        } else {
            sqlHelper.addConditions(s, operator);
        }
        if (sqlHelper.getConditions() == null || sqlHelper.getConditions().isEmpty()) {
            throw new IllegalStateException(String.format("no conditions from %s", s));
        }
        return sqlHelper;
    }

}
