package io.github.bootystar.mybatisplus.generate;

import io.github.bootystar.mybatisplus.generate.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generate.config.impl.ExtraCodeConfig;
import io.github.bootystar.mybatisplus.generate.config.impl.ExtraFiledConfig;
import io.github.bootystar.mybatisplus.generate.generator.core.EnhanceGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.DynamicSqlGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.ExtraCodeGenerator;
import io.github.bootystar.mybatisplus.generate.generator.impl.ExtraFieldGenerator;

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
     * @return {@link EnhanceGenerator }<{@link ExtraFiledConfig.Builder }>
     * @author bootystar
     */
    public static EnhanceGenerator<ExtraFiledConfig.Builder> extraFiledGenerator(String url, String username, String password) {
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
