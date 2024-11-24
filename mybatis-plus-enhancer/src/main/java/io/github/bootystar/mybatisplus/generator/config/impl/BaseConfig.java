package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigBase;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class BaseConfig extends CustomConfigBase {

    public static class Builder extends CustomConfigBase.Builder<BaseConfig, Builder> {
        @Override
        protected BaseConfig initConfig() {
            return new BaseConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }
    }
}
