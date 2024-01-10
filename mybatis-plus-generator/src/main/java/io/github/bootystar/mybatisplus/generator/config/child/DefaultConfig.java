package io.github.bootystar.mybatisplus.generator.config.child;

import io.github.bootystar.mybatisplus.generator.config.ConfigBase;
import io.github.bootystar.mybatisplus.generator.config.ConfigBaseBuilder;

/**
 * @author booty
 *
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
