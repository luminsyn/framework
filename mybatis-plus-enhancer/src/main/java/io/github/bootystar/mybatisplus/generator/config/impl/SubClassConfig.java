package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigBase;
import lombok.Getter;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class SubClassConfig extends CustomConfigBase {

    /**
     * 显示 service impl方法
     */
    protected boolean showServiceImplMethod = true;

    /**
     * 显示mapper方法
     */
    protected boolean showMapperMethod = true;

    /**
     * 构造器
     *
     * @author bootystar
     */
    public static class Builder extends CustomConfigBase.Builder<SubClassConfig, Builder> {

        @Override
        protected SubClassConfig initConfig() {
            return new SubClassConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }

        /**
         * 不生成服务impl的父类方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public Builder disableServiceImplOverrideMethod() {
            this.config.showServiceImplMethod = false;
            return this;
        }

        /**
         * 不生成mapper的父类方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public Builder disableMapperOverrideMethod() {
            this.config.showMapperMethod = false;
            return this;
        }
    }

}


