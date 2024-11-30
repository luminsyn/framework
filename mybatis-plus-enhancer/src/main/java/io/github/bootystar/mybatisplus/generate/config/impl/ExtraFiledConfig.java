package io.github.bootystar.mybatisplus.generate.config.impl;

import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper4ExtraField;
import io.github.bootystar.mybatisplus.generate.config.base.CustomConfigEnhance;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import lombok.Getter;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class ExtraFiledConfig extends CustomConfigEnhance {

    {
        mapperDTO = new ClassInfo(SqlHelper4ExtraField.class);
    }


    public static class Builder extends CustomConfigEnhance.Builder<ExtraFiledConfig, Builder> {

        @Override
        protected ExtraFiledConfig getConfig() {
            return new ExtraFiledConfig();
        }

        @Override
        protected Builder getBuilder() {
            return this;
        }



    }
}


