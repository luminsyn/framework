package io.github.bootystar.mybatisplus.injection;

import io.github.bootystar.mybatisplus.injection.entity.Condition;
import io.github.bootystar.mybatisplus.injection.entity.RecursivelyCondition;
import io.github.bootystar.mybatisplus.injection.entity.Sort;
import io.github.bootystar.mybatisplus.injection.enums.Connector;
import io.github.bootystar.mybatisplus.injection.enums.Operator;
import io.github.bootystar.mybatisplus.util.ReflectUtil;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Field;
import java.util.*;


/**
 * 注入器
 *
 * @author bootystar
 */

@EqualsAndHashCode(callSuper = true)
@Data
@Slf4j
public class Injector extends RecursivelyCondition {


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
     * @return {@link Injector }
     * @author bootystar
     */
    @SneakyThrows
    public Injector requiredConditions(Object entity, Operator operator) {
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
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector requiredConditions(Condition... conditions) {
        return requiredConditions(Arrays.asList(conditions));
    }

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     *
     * @param conditions 条件
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector requiredConditions(List<Condition> conditions) {
        RecursivelyCondition conditionO = getChild();
        RecursivelyCondition conditionN = new RecursivelyCondition();
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
     * @return {@link Injector }
     * @author bootystar
     */
    @SneakyThrows
    public Injector addConditions(Object entity, Operator operator) {
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
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector addConditions(Condition... conditions) {
        return addConditions(Arrays.asList(conditions));
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param conditions 条件
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector addConditions(List<Condition> conditions) {
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
     * 根据此注入器, 获取字段安全的注入器
     * 和现有条件同等优先级
     *
     * @param entityClass 实体类
     * @return {@link SafetyInjector }<{@link T }>
     * @author bootystar
     */
    public <T> SafetyInjector<T> safetyInjector(Class<T> entityClass) {
        return new SafetyInjector<>(this, entityClass);
    }


    /**
     * 安全注入器
     * 根据指定泛型检查字段, 将会过滤不存在的字段
     *
     * @author bootystar
     */
    @Getter
    public static final class SafetyInjector<T> extends RecursivelyCondition {
        /**
         * 实体类
         */
        private final Class<T> entityClass;
        /**
         * 排序条件列表
         */
        private List<Sort> sorts;


        private SafetyInjector(Class<T> entityClass) {
            this.entityClass = entityClass;
        }

        private SafetyInjector(Injector injector, Class<T> entityClass) {
            this.entityClass = entityClass;
            antiInjection(injector);
        }


        private void antiInjection(Injector injector) {
            if (entityClass == null) {
                throw new InjectException("entityClass class can not be null, please check your configuration");
            }
            Map<String, String> map = ReflectUtil.injectableFieldsMap(entityClass);
            if (map.isEmpty()) {
                throw new InjectException("entityClass has no field to convert, please check your configuration");
            }

            String className = entityClass.getName();
            log.debug("start create anti-injection conditions: source class {}", className);
            SafetyInjector<T> childN = this;
            RecursivelyCondition childO = injector;
            while (childO.getConditions() != null && !childO.getConditions().isEmpty()) {
                List<Condition> conditionsN = replaceCondition(childO.getConditions(), map);
                if (conditionsN != null && !conditionsN.isEmpty()) {
                    childN.conditions = conditionsN;
                }
                childO = childO.getChild();
                if (childO == null) {
                    break;
                }
                SafetyInjector<T> child = new SafetyInjector<>(entityClass);
                childN.child = child;
                childN = child;
            }
            this.sorts = replaceSorts(injector.getSorts(), map);
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
                connector = connector.toLowerCase();
                if (!connector.matches("(?i)(AND|OR)")) {
                    throw new InjectException("connector must be <AND> or <OR>, please check");
                }

                if (operator == null || operator.isEmpty()) {
                    operator = "=";
                }
                if (!operator.matches("(?i)(=|>|<|!=|>=|<=|LIKE|NOT LIKE|IS NULL|IS NOT NULL|IN|NOT IN)")) {
                    throw new InjectException("illegal argument ,  operator can't be : " + operator);
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
                    }else {
                        log.debug("condition field [{}] requires collection but value is not iterable, it will be removed", field);
                        continue;
                    }
                }
                if (value != null && operator.matches("(?i)(LIKE|NOT LIKE)")) {
                    if (!(value instanceof String)) {
                        throw new InjectException("condition field [{}] requires string value, but value is : " + value);
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
                Boolean asc = sortO.getAsc();
                if (asc == null) {
                    asc = false;
                }
                if (field == null || field.isEmpty()) {
                    log.debug("sort field [{}] is null , it will be removed", field);
                    sit.remove();
                    continue;
                }
                String jdbcColumn = map.get(field);
                if (jdbcColumn == null) {
                    log.debug("sort field [{}] not exist in injectMap , it will be removed", field);
                    sit.remove();
                }
                list.add(new Sort.ImmutableSort(jdbcColumn, asc));
            }
            return list;
        }
    }


}
