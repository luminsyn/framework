package io.github.bootystar.mybatisplus.generator.config.impl;

import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigEnhance;
import io.github.bootystar.mybatisplus.logic.dynamic.core.SqlHelper;
import io.github.bootystar.mybatisplus.logic.dynamic.core.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.generator.config.info.ClassInfo;
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
        mapperDTO = new ClassInfo(UnmodifiableSqlHelper.class);
    }

    public static class Builder extends CustomConfigEnhance.Builder<DynamicSqlConfig, DynamicSqlConfig.Builder> {
        @Override
        protected DynamicSqlConfig initConfig() {
            return new DynamicSqlConfig();
        }

        @Override
        protected DynamicSqlConfig.Builder initBuilder() {
            return this;
        }
    }


}


