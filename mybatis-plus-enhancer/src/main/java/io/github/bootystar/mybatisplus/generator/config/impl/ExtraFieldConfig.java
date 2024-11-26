package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigEnhance;
import lombok.Getter;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class ExtraFieldConfig extends CustomConfigEnhance {
    {
        this.showServiceImplMethod = true;
        // todo SelectDTO Class
    }

    public static class Builder extends CustomConfigEnhance.Builder<ExtraFieldConfig, ExtraFieldConfig.Builder> {

        @Override
        protected ExtraFieldConfig initConfig() {
            return new ExtraFieldConfig();
        }

        @Override
        protected ExtraFieldConfig.Builder initBuilder() {
            return this;
        }
    }
}


