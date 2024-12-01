package io.github.bootystar.mybatisplus.enhance.helper.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.enums.SqlExtraSuffix;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.ConditionU;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public class ExtraFieldSqlHelper<T> extends UnmodifiableSqlHelper<T> {

    private Map<String, String> suffix2OperatorMap = SqlExtraSuffix.DEFAULT_MAP;

    public ExtraFieldSqlHelper(ISqlTree tree, Class<T> entityClass) {
        super(tree, entityClass);
    }

    private ExtraFieldSqlHelper(ISqlTree tree, Class<T> entityClass, Map<String, String> suffix2OperatorMap) {
        super(entityClass);
        this.suffix2OperatorMap = suffix2OperatorMap;
        initProperties(tree);
    }


    @Override
    protected Collection<ConditionU> validatedConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<ConditionU> result = new ArrayList<>();
        HashMap<String, Object> illegalConditionMap = new HashMap<>();
        Set<String> suffixes = suffix2OperatorMap.keySet();
        for (ISqlCondition conditionO : conditions) {
            String field = conditionO.getField();
            String jdbcColumn = field2JdbcColumnMap.get(field);
            Optional<ConditionU> optional = Optional.empty();
            if (jdbcColumn == null) {
                for (String suffix : suffixes) {
                    if (field.endsWith(suffix)) {
                        String sourceFiled = field.substring(0, field.length() - suffix.length());
                        String operator = suffix2OperatorMap.get(suffix);
                        optional = wrap2JdbcColumnCondition(conditionO.isOr(), sourceFiled, operator, conditionO.getValue());
                        break;
                    }
                }
                if (optional.isPresent()) {
                    optional.ifPresent(result::add);
                } else {
                    illegalConditionMap.put(field, conditionO.getValue());
                }
                continue;
            }
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        if (!illegalConditionMap.isEmpty()) {
            this.paramMap = Collections.unmodifiableMap(illegalConditionMap);
        }
        return result;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private static final String SUFFIX_PATTERN = "^[a-zA-Z0-9_$]+$";
        private HashMap<String, String> suffix2OperatorMap = new HashMap<>();

        private Builder() {

        }

        private void check(String suffix) {
            if (suffix == null) {
                throw new IllegalArgumentException("suffix can't be null");
            }
            if (suffix2OperatorMap == null) {
                throw new IllegalStateException("this build has been built, please recreate a new one");
            }
            if (!suffix.matches(SUFFIX_PATTERN)) {
                throw new IllegalArgumentException("illegal suffix [" + suffix + "] , field names cannot contain special characters");
//                throw new IllegalArgumentException("illegal suffix [" + suffix + "] , it does not match the regular expression:" + SUFFIX_PATTERN);
            }
        }

        /**
         * 不等于
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder ne(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.NE.keyword);
            return this;
        }

        /**
         * 大于
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder gt(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.GT.keyword);
            return this;
        }

        /**
         * 大于等于
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder ge(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.GE.keyword);
            return this;
        }

        /**
         * 小于
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder lt(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.LT.keyword);
            return this;
        }

        /**
         * 小于等于
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder le(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.LE.keyword);
            return this;
        }

        /**
         * 模糊匹配
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder like(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.LIKE.keyword);
            return this;
        }

        /**
         * 不模糊匹配
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder notLike(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.NOT_LIKE.keyword);
            return this;
        }

        /**
         * in
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder in(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.IN.keyword);
            return this;
        }

        /**
         * not in
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder notIn(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.NOT_IN.keyword);
            return this;
        }

        /**
         * is null
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder isNull(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.IS_NULL.keyword);
            return this;
        }

        /**
         * is not null
         *
         * @param suffix 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder isNotNull(String suffix) {
            check(suffix);
            suffix2OperatorMap.put(suffix, SqlKeyword.IS_NOT_NULL.keyword);
            return this;
        }

        /**
         * 构建
         *
         * @param sqlTree 条件树
         * @param clazz   实体类
         * @return {@link ExtraFieldSqlHelper }<{@link T }>
         * @author bootystar
         */
        public <T> ExtraFieldSqlHelper<T> build(ISqlTree sqlTree, Class<T> clazz) {
            if (suffix2OperatorMap == null) {
                throw new IllegalStateException("this build has been built, please recreate a new one");
            }
            HashMap<String, String> map = suffix2OperatorMap;
            this.suffix2OperatorMap = null;
//            return new ExtraFieldSqlHelper<>(sqlTree, clazz, map);
            return null;
        }

    }

}
