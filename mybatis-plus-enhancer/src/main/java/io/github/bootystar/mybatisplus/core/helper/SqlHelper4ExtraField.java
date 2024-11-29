package io.github.bootystar.mybatisplus.core.helper;

import io.github.bootystar.mybatisplus.core.enums.SqlFieldSuffix;
import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.core.param.base.ISqlCondition;
import io.github.bootystar.mybatisplus.core.param.base.ISqlTree;
import io.github.bootystar.mybatisplus.core.param.unmodifiable.ConditionU;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;

import java.util.*;

/**
 * @author bootystar
 */
@Slf4j
@Accessors(chain = true)
public class SqlHelper4ExtraField<T> extends SqlHelper2Generic<T> {

    /**
     * 不等于
     */
    @Setter
    private String suffix4Ne = SqlFieldSuffix.NE.keyword;

    /**
     * 大于
     */
    @Setter
    private String suffix4Gt = SqlFieldSuffix.GT.keyword;

    /**
     * 大于等于
     */
    @Setter
    private String suffix4Ge = SqlFieldSuffix.GE.keyword;

    /**
     * 小于
     */
    @Setter
    private String suffix4Lt = SqlFieldSuffix.LT.keyword;

    /**
     * 小于等于
     */
    @Setter
    private String suffix4Le = SqlFieldSuffix.LE.keyword;

    /**
     * 模糊匹配
     */
    @Setter
    private String suffix4Like = SqlFieldSuffix.LIKE.keyword;

    /**
     * 不模糊匹配
     */
    @Setter
    private String suffix4NotLike = SqlFieldSuffix.NOT_LIKE.keyword;

    /**
     * 批量in
     */
    @Setter
    private String suffix4In = SqlFieldSuffix.IN.keyword;

    /**
     * 批量not in
     */
    @Setter
    private String suffix4NotIn = SqlFieldSuffix.NOT_IN.keyword;

    /**
     * 空字段
     */
    @Setter
    private String suffix4IsNull = SqlFieldSuffix.IS_NULL.keyword;

    /**
     * 非空字段
     */
    @Setter
    private String suffix4IsNotNull = SqlFieldSuffix.IS_NOT_NULL.keyword;

    /**
     * 额外参数
     */
    @Getter
    private Map<String, Object> params;

    public SqlHelper4ExtraField(ISqlTree tree, Class<T> entityClass) {
        super(tree, entityClass);
    }

    public static <T> SqlHelper4ExtraField<T> of(ISqlTree tree, Class<T> entityClass) {
        return new SqlHelper4ExtraField<>(tree, entityClass);
    }

    @Override
    protected Collection<ConditionU> validatedConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return null;
        }
        HashMap<String, Object> paramMap = new HashMap<>();
        ArrayList<ConditionU> result = new ArrayList<>();
        for (ISqlCondition conditionO : conditions) {
            String field = conditionO.getField();
            String jdbcColumn = field2JdbcColumnMap.get(field);
            if (jdbcColumn == null) {
                Optional<ConditionU> unmodifiableCondition = wrapExtraCondition(conditionO);
                if (unmodifiableCondition.isPresent()) {
                    result.add(unmodifiableCondition.get());
                } else {
                    paramMap.put(field, conditionO.getValue());
                }
                continue;
            }
            wrap2JdbcColumnCondition(conditionO).ifPresent(result::add);
        }
        if (!paramMap.isEmpty()) {
            this.params = Collections.unmodifiableMap(paramMap);
        }
        return result;
    }

    protected Optional<ConditionU> wrapExtraCondition(ISqlCondition conditionO) {
        String field = conditionO.getField();
        Object value = conditionO.getValue();
        if (field.endsWith(suffix4Ne)) {
            String sourceField = field.substring(0, field.length() - suffix4Ne.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.NE.keyword, value);
        }
        if (field.endsWith(suffix4Gt)) {
            String sourceField = field.substring(0, field.length() - suffix4Gt.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.GT.keyword, value);
        }
        if (field.endsWith(suffix4Ge)) {
            String sourceField = field.substring(0, field.length() - suffix4Ge.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.GE.keyword, value);
        }
        if (field.endsWith(suffix4Lt)) {
            String sourceField = field.substring(0, field.length() - suffix4Lt.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.LT.keyword, value);
        }
        if (field.endsWith(suffix4Le)) {
            String sourceField = field.substring(0, field.length() - suffix4Le.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.LE.keyword, value);
        }
        if (field.endsWith(suffix4Like)) {
            String sourceField = field.substring(0, field.length() - suffix4Like.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.LIKE.keyword, value);
        }
        if (field.endsWith(suffix4NotLike)) {
            String sourceField = field.substring(0, field.length() - suffix4NotLike.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.NOT_LIKE.keyword, value);
        }
        if (field.endsWith(suffix4In)) {
            String sourceField = field.substring(0, field.length() - suffix4In.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.IN.keyword, value);
        }
        if (field.endsWith(suffix4NotIn)) {
            String sourceField = field.substring(0, field.length() - suffix4NotIn.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.NOT_IN.keyword, value);
        }
        if (field.endsWith(suffix4IsNull)) {
            String sourceField = field.substring(0, field.length() - suffix4IsNull.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.IS_NULL.keyword, value);
        }
        if (field.endsWith(suffix4IsNotNull)) {
            String sourceField = field.substring(0, field.length() - suffix4IsNotNull.length());
            if (field2JdbcColumnMap.get(sourceField) == null) {
                return Optional.empty();
            }
            return wrap2JdbcColumnCondition(SqlKeyword.AND.keyword, field, SqlKeyword.IS_NOT_NULL.keyword, value);
        }
        return Optional.empty();
    }

}
