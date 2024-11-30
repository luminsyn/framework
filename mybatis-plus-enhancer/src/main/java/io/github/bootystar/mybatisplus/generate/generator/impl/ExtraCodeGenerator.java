package io.github.bootystar.mybatisplus.generate.generator.impl;

import io.github.bootystar.mybatisplus.generate.config.impl.BaseConfig;
import io.github.bootystar.mybatisplus.generate.generator.core.AbstractGenerator;

/**
 * 额外代码生成器
 * 通过生成额外代码及属性实现逻辑
 *
 * @author bootystar
 */
public class ExtraCodeGenerator extends AbstractGenerator<BaseConfig, BaseConfig.Builder> {

    public ExtraCodeGenerator(String url, String username, String password) {
        super(url, username, password, new BaseConfig.Builder());
    }

}
