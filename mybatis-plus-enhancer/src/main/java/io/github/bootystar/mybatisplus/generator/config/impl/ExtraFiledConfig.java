package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.core.enums.SqlFieldSuffix;
import io.github.bootystar.mybatisplus.core.helper.SqlHelper4ExtraField;
import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigEnhance;
import io.github.bootystar.mybatisplus.generator.info.ClassInfo;
import lombok.Getter;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class ExtraFiledConfig extends CustomConfigEnhance {

    {
        mapperDTO = new ClassInfo(SqlHelper4ExtraField.class);
    }

    /**
     * 不等于
     */
    protected String suffix4Ne = SqlFieldSuffix.NE.keyword;

    /**
     * 等于
     */
    protected String suffix4Eq = SqlFieldSuffix.EQ.keyword;

    /**
     * 大于
     */
    protected String suffix4Gt = SqlFieldSuffix.GT.keyword;

    /**
     * 大于等于
     */
    protected String suffix4Ge = SqlFieldSuffix.GE.keyword;

    /**
     * 小于
     */
    protected String suffix4Lt = SqlFieldSuffix.LT.keyword;

    /**
     * 小于等于
     */
    protected String suffix4Le = SqlFieldSuffix.LE.keyword;

    /**
     * like
     */
    protected String suffix4Like = SqlFieldSuffix.LIKE.keyword;

    /**
     * not like
     */
    protected String suffix4NotLike = SqlFieldSuffix.NOT_LIKE.keyword;

    /**
     * in
     */
    protected String suffix4In = SqlFieldSuffix.IN.keyword;

    /**
     * not in
     */
    protected String suffix4NotIn = SqlFieldSuffix.NOT_IN.keyword;

    /**
     * is null
     */
    protected String suffix4IsNull = SqlFieldSuffix.IS_NULL.keyword;

    /**
     * is not null
     */
    protected String suffix4IsNotNull = SqlFieldSuffix.IS_NOT_NULL.keyword;



    public static class Builder extends CustomConfigEnhance.Builder<ExtraFiledConfig, Builder> {

        @Override
        protected ExtraFiledConfig initConfig() {
            return new ExtraFiledConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }




    }
}


