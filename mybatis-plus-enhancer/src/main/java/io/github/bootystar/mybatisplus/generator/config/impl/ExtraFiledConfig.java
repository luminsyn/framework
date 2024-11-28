package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigEnhance;
import io.github.bootystar.mybatisplus.generator.info.ClassInfo;
import lombok.Getter;

import java.util.Map;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class ExtraFiledConfig extends CustomConfigEnhance {

    {
        mapperDTO = new ClassInfo(Map.class);
    }

    public static class Builder extends CustomConfigEnhance.Builder<ExtraFiledConfig, Builder> {

        @Override
        protected ExtraFiledConfig initConfig() {
            return new ExtraFiledConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }
    }
}


