package io.github.bootystar.mybatisplus.injection;

import lombok.Data;
import lombok.Setter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * 注入器
 *
 * @author booty
 */
@Slf4j
public class Injector {

    private final static String REMOVE_NOTIFY = "it will be removed";
    /**
     * 条件初始化
     */
    private boolean init = false;

    /**
     * 条件连接符号(or,and)
     */
    @Setter
    private String connector = "and";

    /**
     * 搜索条件列表
     */
    @Setter
    private List<Condition> conditions;

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
        if (!init) throw new AntiInjectException("injector not init, please call init first");
    }

    @SneakyThrows
    public Injector init(Class<Injectable> injectable) {
        if (init) return this;
        if (injectable == null){
            throw new AntiInjectException("injectable class can not be null, please check your configuration");
        }
        if (connector == null || connector.isEmpty()) {
            connector= "and";
        }
        connector=connector.toLowerCase();
        if (!connector.matches("(?i)(and|or)")){
            throw new AntiInjectException("injector connector must be and or or, please check your configuration");
        }
        
        String className = injectable.getName();
        Map<String, String> map = injectable.getConstructor().newInstance().injectMap();
        if (conditions != null) {
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
                    log.debug("condition field [{}] not exist in [{}]'s injectMap, " + REMOVE_NOTIFY, field, className);
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
        }
        if (sorts != null) {
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
                    log.debug("sort field [{}] not exist in [{}] injectMap , " + REMOVE_NOTIFY, field, className);
                    sit.remove();
                }
                sort.setField(jdbcColumn);
                if (sort.getAsc() == null) sort.setAsc(false);
            }
        }
        init = true;
        return this;
    }


    @Data
    public static class Condition {
        private static final String PATTERN = "(?i)(=|>|<|!=|>=|<=|like|not like|is null|is not null|in|not in)";

        /**
         * 字段
         */
        private String field;
        /**
         * 条件(=,>,<,!=,>=,<=,like,not like,is null,is not null,in,not in)
         */
        private String symbol;

        public String getSymbol() {
            if (symbol == null || symbol.isEmpty()) return null;
            if (symbol.matches(PATTERN)) {
                return symbol.toLowerCase();
            }
            return null;
        }

        /**
         * 值
         */
        private Object value;

    }

    @Data
    public static class Sort {
        /**
         * 字段
         */
        private String field;
        /**
         * 是否正序
         */
        private Boolean asc;
    }

}
