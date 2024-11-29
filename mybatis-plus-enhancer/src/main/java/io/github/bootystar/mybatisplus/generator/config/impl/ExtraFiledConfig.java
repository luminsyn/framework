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
    protected String suffix4Ne;

    /**
     * 大于
     */
    protected String suffix4Gt;

    /**
     * 大于等于
     */
    protected String suffix4Ge;

    /**
     * 小于
     */
    protected String suffix4Lt;

    /**
     * 小于等于
     */
    protected String suffix4Le;

    /**
     * like
     */
    protected String suffix4Like;

    /**
     * not like
     */
    protected String suffix4NotLike;

    /**
     * in
     */
    protected String suffix4In;

    /**
     * not in
     */
    protected String suffix4NotIn;

    /**
     * is null
     */
    protected String suffix4IsNull;

    /**
     * is not null
     */
    protected String suffix4IsNotNull;

    public static class Builder extends CustomConfigEnhance.Builder<ExtraFiledConfig, Builder> {

        @Override
        protected ExtraFiledConfig initConfig() {
            return new ExtraFiledConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }


        /**
         * 不等于追加字段后缀
         *
         * @param suffix4Ne 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4Ne(String suffix4Ne) {
            this.config.suffix4Ne = suffix4Ne;
            return this;
        }

        /**
         * 大于追加字段后缀
         *
         * @param suffix4Gt 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4Gt(String suffix4Gt) {
            this.config.suffix4Gt = suffix4Gt;
            return this;
        }

        /**
         * 大于等于追加字段后缀
         *
         * @param suffix4Ge 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4Ge(String suffix4Ge) {
            this.config.suffix4Ge = suffix4Ge;
            return this;
        }

        /**
         * 小于追加字段后缀
         *
         * @param suffix4Lt 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4Lt(String suffix4Lt) {
            this.config.suffix4Lt = suffix4Lt;
            return this;
        }

        /**
         * 小于等于追加字段后缀
         *
         * @param suffix4Le 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4Le(String suffix4Le) {
            this.config.suffix4Le = suffix4Le;
            return this;
        }

        /**
         * like追加字段后缀
         *
         * @param suffix4Like 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4Like(String suffix4Like) {
            this.config.suffix4Like = suffix4Like;
            return this;
        }

        /**
         * not like追加字段后缀
         *
         * @param suffix4NotLike 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4NotLike(String suffix4NotLike) {
            this.config.suffix4NotLike = suffix4NotLike;
            return this;
        }

        /**
         * in追加字段后缀
         *
         * @param suffix4In 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4In(String suffix4In) {
            this.config.suffix4In = suffix4In;
            return this;
        }

        /**
         * not in追加字段后缀
         *
         * @param suffix4NotIn 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4NotIn(String suffix4NotIn) {
            this.config.suffix4NotIn = suffix4NotIn;
            return this;
        }

        /**
         * is null追加字段后缀
         *
         * @param suffix4IsNull 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4IsNull(String suffix4IsNull) {
            this.config.suffix4IsNull = suffix4IsNull;
            return this;
        }

        /**
         * is not null追加字段后缀
         *
         * @param suffix4IsNotNull 后缀
         * @return {@link Builder }
         * @author bootystar
         */
        public Builder suffix4IsNotNull(String suffix4IsNotNull) {
            this.config.suffix4IsNotNull = suffix4IsNotNull;
            return this;
        }




    }
}


