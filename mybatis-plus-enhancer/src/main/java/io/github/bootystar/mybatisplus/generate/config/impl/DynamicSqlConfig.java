package io.github.bootystar.mybatisplus.generate.config.impl;

import io.github.bootystar.mybatisplus.generate.config.base.CustomConfigEnhance;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicSqlHelper;
import io.github.bootystar.mybatisplus.generate.info.ClassInfo;
import lombok.Getter;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class DynamicSqlConfig extends CustomConfigEnhance {

    {
        selectDTO = new ClassInfo(SqlHelper.class);
        mapperDTO = new ClassInfo(DynamicSqlHelper.class);
    }

    public static class Builder extends CustomConfigEnhance.Builder<DynamicSqlConfig, Builder> {

        @Override
        protected DynamicSqlConfig getConfig() {
            return new DynamicSqlConfig();
        }

        @Override
        protected Builder getBuilder() {
            return this;
        }


    }


}


