package io.github.bootystar.mybatisplus.generator;

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
//        // 自3.5.6废弃
//        templateConfigBuilder
//                .mapper("/crud/mapper.java")
//                .service("/crud/service.java")
//                .serviceImpl("/crud/serviceImpl.java")
//        ;
        
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/crud/service.java")
                .serviceImplTemplate("/crud/serviceImpl.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/crud/mapper.java")
        ;
        
    }
    
}
