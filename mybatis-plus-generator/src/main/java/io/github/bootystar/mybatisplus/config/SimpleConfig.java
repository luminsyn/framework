package io.github.bootystar.mybatisplus.config;

import io.github.bootystar.mybatisplus.config.base.ConfigBase;
import io.github.bootystar.mybatisplus.config.base.ConfigBaseBuilder;

/**
 * 默认配置类
 *
 * @author bootystar
 */
public class SimpleConfig extends ConfigBase {

    public SimpleConfig() {
        super(SimpleConfig.class);
    }

    public static class Builder extends ConfigBaseBuilder<SimpleConfig, Builder> {
        @Override
        protected SimpleConfig initConfig() {
            return new SimpleConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }
    }
}
