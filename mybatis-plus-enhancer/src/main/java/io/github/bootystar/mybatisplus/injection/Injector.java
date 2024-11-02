package io.github.bootystar.mybatisplus.injection;

import io.github.bootystar.mybatisplus.util.ReflectUtil;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;


/**
 * 注入器
 *
 * @author bootystar
 */
@Slf4j
public class Injector {

    private final static String REMOVE_NOTIFY = "it will be removed";

    /**
     * 条件初始化标记
     */
    private boolean init = false;

    /**
     * 条件连接符号(or,and)
     * 默认and
     */
    @Setter
    private String connector = "and";

    /**
     * 搜索条件列表
     */
    @Setter
    private List<Condition> conditions;

    /**
     * 前置条件
     * (必定生效的选择条件)
     */
    @Getter
    private List<Condition> requiredConditions;

    /**
     * 排序条件列表
     */
    @Setter
    private List<Sort> sorts;

    public String getConnector() {
        initCheck();
        return connector;
    }

    public List<Condition> getConditions() {
        initCheck();
        return conditions;
    }

    public List<Sort> getSorts() {
        initCheck();
        return sorts;
    }

    private void initCheck() {
        if (!init) throw new InjectException("injector not init, please call init first");
    }

    /**
     * 初始化
     *
     * @param entityClass 数据库对应实体类
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector init(Class<?> entityClass) {
        if (init) return this;
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
        init = true;
        return this;
    }

    /**
     * 添加必须条件
     * @see #requiredCondition(List)
     *
     * @param conditions 条件
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector requiredCondition(Condition... conditions) {
        return requiredCondition(Arrays.asList(conditions));
    }

    /**
     * 添加必须条件
     * 例:
     * select * from table where 字段1=值1 or 字段2=值2 ...
     * 会优化为
     * select * from table where 必须字敦1=必须值1 and 必须字段2=必须值2  and (字段1=值1 or 字段2=值2 ...)
     *
     * @param conditions 条件
     * @return {@link Injector }
     * @author bootystar
     */
    public Injector requiredCondition(List<Condition> conditions) {
        if (this.requiredConditions==null) {
            this.requiredConditions = new ArrayList<>();
        }
        this.requiredConditions.addAll(conditions);
        return this;
    }



    private List<Condition> replaceCondition(List<Condition> conditions, Map<String, String> map) {
        Iterator<Condition> cit = conditions.iterator();
        while (cit.hasNext()) {
            Condition condition = cit.next();
            String field = condition.getField();
            String symbol = condition.getSymbol();
            if (field == null || field.isEmpty() || symbol == null || symbol.isEmpty()) {
                cit.remove();
                continue;
            }
            String jdbcColumn = map.get(field);
            if (jdbcColumn == null) {
                log.debug("condition field [{}] not exist in injectMap, " + REMOVE_NOTIFY, field);
                cit.remove();
            }
            condition.setField(jdbcColumn);
            Object value = condition.getValue();
            if (symbol.matches("(?i)(in|not in)")) {
                if (value == null) {
                    log.debug("condition field [{}] requires collection but value is null, " + REMOVE_NOTIFY, field);
                    cit.remove();
                    continue;
                }
                if (value instanceof Iterable) {
                    Iterable<?> iterable = (Iterable<?>) value;
                    if (!iterable.iterator().hasNext()) {
                        log.debug("condition field [{}] requires collection but value is empty, " + REMOVE_NOTIFY, field);
                        cit.remove();
                        continue;
                    }
                }
            }
            if (value == null && !symbol.matches("(?i)(is null|is not null)")) {
                log.debug("condition field [{}] requires value but value is null, " + REMOVE_NOTIFY, field);
                cit.remove();
                continue;
            }
        }
        return conditions;
    }

    public static List<Sort> replaceSort(List<Sort> sorts, Map<String, String> map) {
        Iterator<Sort> sit = sorts.iterator();
        while (sit.hasNext()) {
            Sort sort = sit.next();
            String field = sort.getField();
            if (field == null || field.isEmpty()) {
                log.debug("sort field [{}] is null , " + REMOVE_NOTIFY, field);
                sit.remove();
                continue;
            }
            String jdbcColumn = map.get(field);
            if (jdbcColumn == null) {
                log.debug("sort field [{}] not exist in injectMap , " + REMOVE_NOTIFY, field);
                sit.remove();
            }
            sort.setField(jdbcColumn);
            if (sort.getAsc() == null) sort.setAsc(false);
        }
        return sorts;
    }


}
