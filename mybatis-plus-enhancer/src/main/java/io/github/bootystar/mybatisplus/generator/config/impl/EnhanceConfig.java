package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigEnhance;
import io.github.bootystar.mybatisplus.generator.config.info.ClassInfo;
import lombok.Getter;

import java.util.Map;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class EnhanceConfig extends CustomConfigEnhance {

    {
        mapperDTO = new ClassInfo(Map.class);
    }

    public static class Builder extends CustomConfigEnhance.Builder<EnhanceConfig, EnhanceConfig.Builder> {

        @Override
        protected EnhanceConfig initConfig() {
            return new EnhanceConfig();
        }

        @Override
        protected EnhanceConfig.Builder initBuilder() {
            return this;
        }
    }
}


