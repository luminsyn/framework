package io.github.bootystar.mybatisplus.generator.config.child;

import io.github.bootystar.mybatisplus.generator.config.ConfigBase;
import io.github.bootystar.mybatisplus.generator.config.ConfigBaseBuilder;

/**
 * 默认配置类
 * @author bootystar
 */
public class DefaultConfig extends ConfigBase {


    public static class Builder extends ConfigBaseBuilder<DefaultConfig, Builder> {
        @Override
        protected DefaultConfig initConfig() {
            return new DefaultConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }
    }
}
