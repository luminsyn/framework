package io.github.bootystar.mybatisplus.injection;

import io.github.bootystar.mybatisplus.injection.entity.Condition;
import io.github.bootystar.mybatisplus.injection.entity.Sort;
import io.github.bootystar.mybatisplus.util.ReflectUtil;
import lombok.Data;
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
@Data
public class Injector {

    /**
     * 条件连接符号(or,and)
     * 默认and
     */
    private String connector = "and";

    /**
     * 自定义条件列表
     */
    private List<Condition> conditions;

    /**
     * 子条件
     * // todo 嵌套子条件的处理
     */
    private List<Condition> children;

    /**
     * 排序条件列表
     */
    private List<Sort> sorts;


    /**
     * 添加前置条件
     *
     * @param conditions 条件
     * @return {@link Injector }
     * @author bootystar
     * @see #requiredCondition(List)
     */
    public Injector requiredCondition(Condition... conditions) {
        return requiredCondition(Arrays.asList(conditions));
    }

    /**
     * 添加前置条件
     * 例:
     * select * from table where 字段1=值1 or 字段2=值2 ...
     * 会优化为
     * select * from table where 前置字段1=前置值1 and 前置字段2=前置值2  and (字段1=值1 or 字段2=值2 ...)
     *
     * @param conditions 条件
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector requiredCondition(List<Condition> conditions) {
        if (this.children == null) {
            this.children = new ArrayList<>();
        }
        this.children.addAll(conditions);
        return this;
    }


    /**
     * 添加前置条件
     * 传入实体的指定属性有值时. 会强制添加对应字段的查询
     *
     * @param entity 实体
     * @return {@link Injector }
     * @author bootystar
     */
    @SneakyThrows
    public Injector requiredCondition(Object entity) {
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
            condition.setSymbol("=");
            condition.setValue(value);
            conditions.add(condition);
        }
        this.children.addAll(conditions);
        return this;
    }


    /**
     * 根据此注入器, 获取字段安全的注入器
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
    @Slf4j
    public static class SafetyInjector<T> {
        /**
         * 实体类
         */
        private final Class<T> entityClass;
        /**
         * 条件连接符号(or,and)
         * 默认and
         */
        @Getter
        private String connector = "and";

        /**
         * 搜索条件列表
         */
        @Getter
        private List<Condition> conditions;

        /**
         * 前置条件
         * (必定生效的选择条件)
         * todo 嵌套子条件
         */
        @Getter
        private List<Condition> requiredConditions;

        /**
         * 排序条件列表
         */
        private List<Sort> sorts;

        private SafetyInjector(Injector injector, Class<T> entityClass) {
            this.entityClass = entityClass;
            ReflectUtil.copyProperties(injector, this);
            antiInjection();
        }

        private void antiInjection() {
            if (entityClass == null) {
                throw new InjectException("entityClass class can not be null, please check your configuration");
            }
            Map<String, String> map = ReflectUtil.injectableFieldsMap(entityClass);
            if (map.isEmpty()) {
                throw new InjectException("entityClass has no field to convert, please check your configuration");
            }
            if (connector == null || connector.isEmpty()) {
                connector = "and";
            }
            connector = connector.toLowerCase();
            if (!connector.matches("(?i)(and|or)")) {
                throw new InjectException("connector must be <and> or <or>, please check");
            }
            String className = entityClass.getName();
            log.debug("start create anti-injection conditions: source class {}", className);
            if (requiredConditions != null) {
                List<Condition> requiredCondition = replaceCondition(requiredConditions, map);
                if (requiredCondition.isEmpty()) {
                    throw new InjectException("requiredCondition field or value has error , please check");
                }
            }
            if (conditions != null) {
                replaceCondition(conditions, map);
            }
            if (sorts != null) {
                replaceSort(sorts, map);
            }
        }

        private static List<Condition> replaceCondition(List<Condition> conditions, Map<String, String> map) {
            Iterator<Condition> cit = conditions.iterator();
            while (cit.hasNext()) {
                Condition condition = cit.next();
                String field = condition.getField();
                String symbol = condition.getSymbol();
                if (symbol == null) {
                    symbol = "=";
                    condition.setSymbol(symbol);
                }
                if (field == null || field.isEmpty() || symbol == null || symbol.isEmpty()) {
                    cit.remove();
                    continue;
                }
                String jdbcColumn = map.get(field);
                if (jdbcColumn == null) {
                    log.debug("condition field [{}] not exist in injectMap, it will be removed", field);
                    cit.remove();
                }
                condition.setField(jdbcColumn);
                Object value = condition.getValue();
                if (symbol.matches("(?i)(in|not in)")) {
                    if (value == null) {
                        log.debug("condition field [{}] requires collection but value is null, it will be removed", field);
                        cit.remove();
                        continue;
                    }
                    if (value instanceof Iterable) {
                        Iterable<?> iterable = (Iterable<?>) value;
                        if (!iterable.iterator().hasNext()) {
                            log.debug("condition field [{}] requires collection but value is empty, it will be removed", field);
                            cit.remove();
                            continue;
                        }
                    }
                }
                if (value == null && !symbol.matches("(?i)(is null|is not null)")) {
                    log.debug("condition field [{}] requires value but value is null, it will be removed", field);
                    cit.remove();
                    continue;
                }
            }
            return conditions;
        }

        private static List<Sort> replaceSort(List<Sort> sorts, Map<String, String> map) {
            Iterator<Sort> sit = sorts.iterator();
            while (sit.hasNext()) {
                Sort sort = sit.next();
                String field = sort.getField();
                Boolean asc = sort.getAsc();
                if (asc == null) {
                    asc = false;
                    sort.setAsc(asc);
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
                sort.setField(jdbcColumn);
            }
            return sorts;
        }
    }


}
