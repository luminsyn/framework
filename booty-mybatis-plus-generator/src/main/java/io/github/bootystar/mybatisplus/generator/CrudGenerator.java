package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.generator.config.ConfigBaseBuilder;
import io.github.bootystar.mybatisplus.generator.config.child.DefaultConfig;

/**
 * 开放式CRUD代码生成器
 *
 * @author booty
 */
public class CrudGenerator extends AbstractGenerator {
    protected DefaultConfig.Builder customConfigBuilder = new DefaultConfig.Builder();
    public DefaultConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }

    public CrudGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();
        templateConfigBuilder.mapper("/crud/mapper.java");
        templateConfigBuilder.service("/crud/service.java");
        templateConfigBuilder.serviceImpl("/crud/serviceImpl.java");
    }
    
}
