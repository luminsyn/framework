package io.github.bootystar.mybatisplus.logic.splicing.dto;

import io.github.bootystar.mybatisplus.logic.splicing.SplicingException;
import io.github.bootystar.mybatisplus.logic.splicing.enums.Connector;
import io.github.bootystar.mybatisplus.logic.splicing.enums.Operator;
import io.github.bootystar.mybatisplus.util.ReflectUtil;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Field;
import java.util.*;

/**
 * SQL拼接器
 *
 * @author bootystar
 */
@EqualsAndHashCode(callSuper = true)
@Data
@Slf4j
public class Splicer extends ConditionR {

    /**
     * 排序条件列表
     */
    protected List<Sort> sorts;

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     *
     * @param entity   实体
     * @param operator 操作符
     * @return {@link Splicer }
     * @author bootystar
     */
    @SneakyThrows
    public Splicer requiredConditions(Object entity, Operator operator) {
        if (entity == null) {
            throw new IllegalStateException("entity is null");
        }
        Map<String, Field> fieldMap = ReflectUtil.fieldMap(entity.getClass());
        List<Condition> conditions = new ArrayList<>();
        for (Field field : fieldMap.values()) {
            Object value = field.get(entity);
            if (value == null) continue;
            Condition condition = new Condition();
            condition.setField(field.getName());
            condition.setOperator(operator.keyword);
            condition.setValue(value);
            conditions.add(condition);
        }
        return requiredConditions(conditions);
    }

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     * 前置条件新>前置条件旧>一般条件
     *
     * @param conditions 条件
     * @return {@link Splicer }
     * @author bootystar
     */
    public Splicer requiredConditions(Condition... conditions) {
        return requiredConditions(Arrays.asList(conditions));
    }

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     *
     * @param conditions 条件
     * @return {@link Splicer }
     * @author bootystar
     */
    public Splicer requiredConditions(List<Condition> conditions) {
        ConditionR conditionO = getChild();
        ConditionR conditionN = new ConditionR();
        setChild(conditionN);
        conditionN.setChild(conditionO);
        conditionN.setConditions(conditions);
        return this;
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param entity   实体
     * @param operator 操作符
     * @return {@link Splicer }
     * @author bootystar
     */
    @SneakyThrows
    public Splicer addConditions(Object entity, Operator operator) {
        if (entity == null) {
            throw new IllegalStateException("entity is null");
        }
        Map<String, Field> fieldMap = ReflectUtil.fieldMap(entity.getClass());
        List<Condition> conditions = new ArrayList<>();
        for (Field field : fieldMap.values()) {
            Object value = field.get(entity);
            if (value == null) continue;
            Condition condition = new Condition();
            condition.setField(field.getName());
            condition.setOperator(operator.keyword);
            condition.setValue(value);
            conditions.add(condition);
        }
        return addConditions(conditions);
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param conditions 条件
     * @return {@link Splicer }
     * @author bootystar
     */
    public Splicer addConditions(Condition... conditions) {
        return addConditions(Arrays.asList(conditions));
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param conditions 条件
     * @return {@link Splicer }
     * @author bootystar
     */
    public Splicer addConditions(List<Condition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        List<Condition> conditionsO = getConditions();
        int size = conditionsO == null ? conditions.size() : conditionsO.size() + conditions.size();
        ArrayList<Condition> conditionsN = new ArrayList<>(size);
        conditionsN.addAll(conditions);
        if (conditionsO != null) {
            conditionsN.addAll(conditionsO);
        }
        setConditions(conditionsN);
        return this;
    }


    /**
     * 根据此拼接器, 获取字段安全的拼接器
     * 和现有条件同等优先级
     *
     * @param entityClass 实体类
     * @return {@link ImmutableSplicer }<{@link T }>
     * @author bootystar
     */
    public <T> ImmutableSplicer<T> immutableSplicer(Class<T> entityClass) {
        return new ImmutableSplicer<>(this, entityClass);
    }


    /**
     * 不可变拼接器
     * 根据指定泛型检查字段, 将会过滤不存在的字段
     *
     * @author bootystar
     */
    @Getter
    public static final class ImmutableSplicer<T> extends ConditionR {
        /**
         * 实体类
         */
        private final Class<T> entityClass;
        /**
         * 排序条件列表
         */
        private List<Sort> sorts;


        private ImmutableSplicer(Class<T> entityClass) {
            this.entityClass = entityClass;
        }

        private ImmutableSplicer(Splicer splicer, Class<T> entityClass) {
            this.entityClass = entityClass;
            fieldCheck(splicer);
        }


        private void fieldCheck(Splicer splicer) {
            if (entityClass == null) {
                throw new SplicingException("entityClass class can not be null, please check your configuration");
            }
            Map<String, String> map = ReflectUtil.injectableFieldsMap(entityClass);
            if (map.isEmpty()) {
                throw new SplicingException("entityClass has no field to convert, please check your configuration");
            }

            String className = entityClass.getName();
            log.debug("start field check ,  source class {}", className);
            ImmutableSplicer<T> childN = this;
            ConditionR childO = splicer;
            while (childO.getConditions() != null && !childO.getConditions().isEmpty()) {
                List<Condition> conditionsN = replaceCondition(childO.getConditions(), map);
                if (conditionsN != null && !conditionsN.isEmpty()) {
                    childN.conditions = conditionsN;
                }
                childO = childO.getChild();
                if (childO == null) {
                    break;
                }
                ImmutableSplicer<T> child = new ImmutableSplicer<>(entityClass);
                childN.child = child;
                childN = child;
            }
            this.sorts = replaceSorts(splicer.getSorts(), map);
        }


        private static List<Condition> replaceCondition(List<Condition> conditions, Map<String, String> map) {
            if (conditions == null || conditions.isEmpty()) {
                return null;
            }
            Iterator<Condition> cit = conditions.iterator();
            ArrayList<Condition> list = new ArrayList<>();
            while (cit.hasNext()) {
                Condition conditionO = cit.next();
                String connector = conditionO.getConnector();
                String field = conditionO.getField();
                String operator = conditionO.getOperator();
                Object value = conditionO.getValue();

                if (connector == null || connector.isEmpty()) {
                    connector = Connector.AND.keyword;
                }
                connector = connector.toUpperCase();

                if (!connector.matches("(?i)(AND|OR)")) {
                    throw new SplicingException("connector must be <AND> or <OR>, please check");
                }

                if (operator == null || operator.isEmpty()) {
                    operator = "=";
                }
                operator = operator.toUpperCase();

                if (!operator.matches("(?i)(=|>|<|!=|>=|<=|LIKE|NOT LIKE|IS NULL|IS NOT NULL|IN|NOT IN)")) {
                    throw new SplicingException("illegal argument ,  operator can't be : " + operator);
                }

                if (field == null || field.isEmpty()) {
                    continue;
                }
                String jdbcColumn = map.get(field);
                if (jdbcColumn == null) {
                    log.debug("condition field [{}] not exist in injectMap, it will be removed", field);
                    continue;
                }
                if (value == null && !operator.matches("(?i)(IS NULL|IS NOT NULl)")) {
                    log.debug("condition field [{}] requires value but value is null, it will be removed", field);
                    continue;
                }
                if (operator.matches("(?i)(IN|NOT IN)")) {
                    if (value instanceof Iterable) {
                        Iterable<?> iterable = (Iterable<?>) value;
                        if (!iterable.iterator().hasNext()) {
                            log.debug("condition field [{}] requires collection but value is empty, it will be removed", field);
                            continue;
                        }
                    } else {
                        log.debug("condition field [{}] requires collection but value is not iterable, it will be removed", field);
                        continue;
                    }
                }
                if (value != null && operator.matches("(?i)(LIKE|NOT LIKE)")) {
                    if (!(value instanceof String)) {
                        throw new SplicingException("condition field [{}] requires string value, but value is : " + value);
                    }
                    value = "%" + value + "%";
                }
                list.add(new Condition.ImmutableCondition(connector, jdbcColumn, operator, value));
            }
            return list;
        }

        private static List<Sort> replaceSorts(List<Sort> sorts, Map<String, String> map) {
            if (sorts == null || sorts.isEmpty()) {
                return null;
            }
            ArrayList<Sort> list = new ArrayList<>();
            Iterator<Sort> sit = sorts.iterator();
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
                list.add(new Sort.ImmutableSort(jdbcColumn, desc));
            }
            return list;
        }
    }


}
