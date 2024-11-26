package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.generator.config.base.CustomConfig;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class ExtraCodeConfig extends CustomConfig {

    public static class Builder extends CustomConfig.Builder<ExtraCodeConfig, Builder> {
        @Override
        protected ExtraCodeConfig initConfig() {
            return new ExtraCodeConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }
    }
}
