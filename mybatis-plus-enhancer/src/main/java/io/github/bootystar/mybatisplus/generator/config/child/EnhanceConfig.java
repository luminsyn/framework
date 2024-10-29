package io.github.bootystar.mybatisplus.generator.config.child;

import io.github.bootystar.mybatisplus.generator.config.ConfigBase;
import io.github.bootystar.mybatisplus.generator.config.ConfigBaseBuilder;
import lombok.Getter;

/**
 * 父类生成器配置类
 * @author booty
 */
@Getter
public class EnhanceConfig extends ConfigBase {

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
     * @author booty
     * @since 2023/12/19
     */
    public static class Builder extends ConfigBaseBuilder<EnhanceConfig, Builder> {

        @Override
        protected EnhanceConfig initConfig() {
            return new EnhanceConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }


        /**
         * 不生成服务impl的父类方法
         *
         * @return {@code U }
         * @author booty
         *
         */
        public Builder disableServiceImplOverrideMethod() {
            this.config.showServiceImplMethod = false;
            return this;
        }

        /**
         * 不生成mapper的父类方法
         *
         * @return {@code U }
         * @author booty
         *
         */
        public Builder disableMapperOverrideMethod() {
            this.config.showMapperMethod = false;
            return this;
        }


    }

}


