package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.generator.config.child.DefaultConfig;

/**
 * 代码注入生成器
 *
 * @author bootystar
 */
public class SimpleGenerator extends AbstractGenerator {
    protected DefaultConfig.Builder customConfigBuilder = new DefaultConfig.Builder();
    public DefaultConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }

    public SimpleGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();
//        // 自3.5.6废弃
//        templateConfigBuilder
//                .mapper("/crud/mapper.java")
//                .service("/crud/service.java")
//                .serviceImpl("/crud/serviceImpl.java")
//        ;
        
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/simple/service.java")
                .serviceImplTemplate("/simple/serviceImpl.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/simple/mapper.java")
        ;
        
    }
    
}
