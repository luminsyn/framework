package io.github.bootystar.mybatisplus.generate;

import io.github.bootystar.mybatisplus.generate.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generate.config.impl.BaseConfig;
import io.github.bootystar.mybatisplus.generate.config.impl.ExtraFiledConfig;
import io.github.bootystar.mybatisplus.generate.generator.core.EnhanceGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.DynamicSqlGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.ExtraCodeGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.ExtraParamGenerator;

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
