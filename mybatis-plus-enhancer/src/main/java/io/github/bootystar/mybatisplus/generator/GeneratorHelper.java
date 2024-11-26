package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.generator.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generator.config.impl.ExtraCodeConfig;
import io.github.bootystar.mybatisplus.generator.config.impl.ExtraFieldConfig;
import io.github.bootystar.mybatisplus.generator.core.base.EnhanceGenerator;
import io.github.bootystar.mybatisplus.generator.core.impl.DynamicSqlGenerator;
import io.github.bootystar.mybatisplus.generator.core.impl.ExtraCodeGenerator;
import io.github.bootystar.mybatisplus.generator.core.impl.ExtraFieldGenerator;

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
     * @return {@link EnhanceGenerator }<{@link ExtraCodeConfig.Builder }>
     * @author bootystar
     */
    public static EnhanceGenerator<ExtraCodeConfig.Builder> extraCodeGenerator(String url, String username, String password) {
        return new ExtraCodeGenerator(url, username, password);
    }

    /**
     * 额外字段生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link EnhanceGenerator }<{@link ExtraFieldConfig.Builder }>
     * @author bootystar
     */
    public static EnhanceGenerator<ExtraFieldConfig.Builder> extraFieldGenerator(String url, String username, String password) {
        return new ExtraFieldGenerator(url, username, password);
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
