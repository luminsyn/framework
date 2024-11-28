package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.generator.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generator.config.impl.BaseConfig;
import io.github.bootystar.mybatisplus.generator.config.impl.ExtraFiledConfig;
import io.github.bootystar.mybatisplus.generator.core.base.EnhanceGenerator;
import io.github.bootystar.mybatisplus.generator.core.impl.DynamicSqlGenerator;
import io.github.bootystar.mybatisplus.generator.core.impl.ExtraCodeGenerator;
import io.github.bootystar.mybatisplus.generator.core.impl.ExtraParamGenerator;

/**
 * @author bootystar
 */
public abstract class GeneratorHelper {

    /**
     * 额外代码生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link EnhanceGenerator }<{@link BaseConfig.Builder }>
     * @author bootystar
     */
    public static EnhanceGenerator<BaseConfig.Builder> extraCodeGenerator(String url, String username, String password) {
        return new ExtraCodeGenerator(url, username, password);
    }

    /**
     * 额外字段生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link EnhanceGenerator }<{@link ExtraFiledConfig.Builder }>
     * @author bootystar
     */
    public static EnhanceGenerator<ExtraFiledConfig.Builder> extraParamGenerator(String url, String username, String password) {
        return new ExtraParamGenerator(url, username, password);
    }

    /**
     * 动态sql生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link EnhanceGenerator }<{@link DynamicSqlConfig.Builder }>
     * @author bootystar
     */
    public static EnhanceGenerator<DynamicSqlConfig.Builder> dynamicSqlGenerator(String url, String username, String password) {
        return new DynamicSqlGenerator(url, username, password);
    }


}
