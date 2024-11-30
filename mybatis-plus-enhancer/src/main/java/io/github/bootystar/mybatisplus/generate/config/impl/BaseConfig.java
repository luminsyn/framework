package io.github.bootystar.mybatisplus.generate.config.impl;

import io.github.bootystar.mybatisplus.generate.config.base.CustomConfig;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class BaseConfig extends CustomConfig {

    public static class Builder extends CustomConfig.Builder<BaseConfig, Builder> {

        @Override
        protected BaseConfig getConfig() {
            return new BaseConfig();
        }

        @Override
        protected Builder getBuilder() {
            return this;
        }

    }
}
