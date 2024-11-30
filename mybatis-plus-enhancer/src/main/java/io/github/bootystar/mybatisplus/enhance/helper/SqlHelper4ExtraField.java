package io.github.bootystar.mybatisplus.enhance.helper;

import io.github.bootystar.mybatisplus.enhance.enums.SqlFieldSuffix;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.unmodifiable.ConditionU;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
public class SqlHelper4ExtraField<T> extends GenericSqlHelper<T> {

    private SuffixBuilder suffixBuilder = SuffixBuilder.DEFAULT;

    public SqlHelper4ExtraField(ISqlTree tree, Class<T> entityClass) {
        super(tree, entityClass);
    }

    public SqlHelper4ExtraField(ISqlTree tree, Class<T> entityClass, SuffixBuilder suffixBuilder) {
        super(entityClass);
        this.suffixBuilder = suffixBuilder;
        initProperties(tree);
    }


    @Override
    protected Collection<ConditionU> validatedConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        ArrayList<ConditionU> result = new ArrayList<>();
        HashMap<String, Object> illegalConditionMap = new HashMap<>();
        for (ISqlCondition conditionO : conditions) {
            String field = conditionO.getField();
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                Optional<ConditionU> extraFieldCondition = wrapExtraCondition(conditionO);
                if (extraFieldCondition.isPresent()) {
                    result.add(extraFieldCondition.get());
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

    protected Optional<ConditionU> wrapExtraCondition(ISqlCondition conditionO) {
        String field = conditionO.getField();
        Object value = conditionO.getValue();
//        if (field.endsWith(suffix4Ne)) {
//            String sourceField = field.substring(0, field.length() - suffix4Ne.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.NE.keyword, value);
//        }
//        if (field.endsWith(suffix4Gt)) {
//            String sourceField = field.substring(0, field.length() - suffix4Gt.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.GT.keyword, value);
//        }
//        if (field.endsWith(suffix4Ge)) {
//            String sourceField = field.substring(0, field.length() - suffix4Ge.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.GE.keyword, value);
//        }
//        if (field.endsWith(suffix4Lt)) {
//            String sourceField = field.substring(0, field.length() - suffix4Lt.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.LT.keyword, value);
//        }
//        if (field.endsWith(suffix4Le)) {
//            String sourceField = field.substring(0, field.length() - suffix4Le.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.LE.keyword, value);
//        }
//        if (field.endsWith(suffix4Like)) {
//            String sourceField = field.substring(0, field.length() - suffix4Like.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.LIKE.keyword, value);
//        }
//        if (field.endsWith(suffix4NotLike)) {
//            String sourceField = field.substring(0, field.length() - suffix4NotLike.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.NOT_LIKE.keyword, value);
//        }
//        if (field.endsWith(suffix4In)) {
//            String sourceField = field.substring(0, field.length() - suffix4In.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.IN.keyword, value);
//        }
//        if (field.endsWith(suffix4NotIn)) {
//            String sourceField = field.substring(0, field.length() - suffix4NotIn.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.NOT_IN.keyword, value);
//        }
//        if (field.endsWith(suffix4IsNull)) {
//            String sourceField = field.substring(0, field.length() - suffix4IsNull.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.IS_NULL.keyword, value);
//        }
//        if (field.endsWith(suffix4IsNotNull)) {
//            String sourceField = field.substring(0, field.length() - suffix4IsNotNull.length());
//            if (field2JdbcColumnMap.get(sourceField) == null) {
//                return Optional.empty();
//            }
//            return wrap2JdbcColumnCondition(conditionO.isOr(), field, SqlKeyword.IS_NOT_NULL.keyword, value);
//        }
        return Optional.empty();
    }

    public static SuffixBuilder suffixBuilder() {
        return new SuffixBuilder();
    }


    /**
     * 扩展字段后缀
     *
     * @author bootystar
     */
    @Getter
    public static class SuffixBuilder {
        private static final SuffixBuilder DEFAULT;

        static {
            DEFAULT = new SuffixBuilder();
            DEFAULT.ne = SqlFieldSuffix.NE.keyword;
            DEFAULT.gt = SqlFieldSuffix.GT.keyword;
            DEFAULT.ge = SqlFieldSuffix.GE.keyword;
            DEFAULT.lt = SqlFieldSuffix.LT.keyword;
            DEFAULT.le = SqlFieldSuffix.LE.keyword;
            DEFAULT.like = SqlFieldSuffix.LIKE.keyword;
            DEFAULT.notLike = SqlFieldSuffix.NOT_LIKE.keyword;
            DEFAULT.in = SqlFieldSuffix.IN.keyword;
            DEFAULT.notIn = SqlFieldSuffix.NOT_IN.keyword;
            DEFAULT.isNull = SqlFieldSuffix.IS_NULL.keyword;
            DEFAULT.isNotNull = SqlFieldSuffix.IS_NOT_NULL.keyword;
        }

        /**
         * 不等于
         */
        private String ne;

        /**
         * 大于
         */
        private String gt;

        /**
         * 大于等于
         */
        private String ge;

        /**
         * 小于
         */
        private String lt;

        /**
         * 小于等于
         */
        private String le;

        /**
         * 模糊匹配
         */
        private String like;

        /**
         * 不模糊匹配
         */
        private String notLike;

        /**
         * IN
         */
        private String in;

        /**
         * NOT IN
         */
        private String notIn;

        /**
         * IS NULL
         */
        private String isNull;

        /**
         * IS NOT NULL
         */
        private String isNotNull;


        /**
         * 不等于
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder ne(String suffix) {
            this.ne = suffix;
            return this;
        }

        /**
         * 大于
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder gt(String suffix) {
            this.gt = suffix;
            return this;
        }

        /**
         * 大于等于
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder ge(String suffix) {
            this.ge = suffix;
            return this;
        }

        /**
         * 小于
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder lt(String suffix) {
            this.lt = suffix;
            return this;
        }

        /**
         * 小于等于
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder le(String suffix) {
            this.le = suffix;
            return this;
        }

        /**
         * 模糊匹配
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder like(String suffix) {
            this.like = suffix;
            return this;
        }

        /**
         * 不模糊匹配
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder notLike(String suffix) {
            this.notLike = suffix;
            return this;
        }

        /**
         * IN
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder in(String suffix) {
            this.in = suffix;
            return this;
        }

        /**
         * NOT IN
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder notIn(String suffix) {
            this.notIn = suffix;
            return this;
        }

        /**
         * IS NULL
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder isNull(String suffix) {
            this.isNull = suffix;
            return this;
        }

        /**
         * IS NOT NULL
         *
         * @param suffix 后缀
         * @return {@link SuffixBuilder }
         * @author bootystar
         */
        public SuffixBuilder isNotNull(String suffix) {
            this.isNotNull = suffix;
            return this;
        }

        public <T> SqlHelper4ExtraField<T> build(ISqlTree tree, Class<T> clazz) {
            return new SqlHelper4ExtraField<>(tree, clazz, this);
        }

    }

}
