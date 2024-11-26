package io.github.bootystar.mybatisplus.generator;

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
     * @return {@link ExtraCodeGenerator }
     * @author bootystar
     */
    public static ExtraCodeGenerator extraCodeGenerator(String url, String username, String password) {
        return new ExtraCodeGenerator(url, username, password);
    }

    /**
     * 额外字段生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link ExtraFieldGenerator }
     * @author bootystar
     */
    public static ExtraFieldGenerator extraFieldGenerator(String url, String username, String password) {
        return new ExtraFieldGenerator(url, username, password);
    }

    /**
     * 动态sql生成器
     *
     * @param url      url
     * @param username 用户名
     * @param password 密码
     * @return {@link DynamicSqlGenerator }
     * @author bootystar
     */
    public static DynamicSqlGenerator dynamicSqlGenerator(String url, String username, String password) {
        return new DynamicSqlGenerator(url, username, password);
    }


}
