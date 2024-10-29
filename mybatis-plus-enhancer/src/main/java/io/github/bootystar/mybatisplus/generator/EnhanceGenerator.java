package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.enhancer.EnhanceMapper;
import io.github.bootystar.mybatisplus.enhancer.EnhanceService;
import io.github.bootystar.mybatisplus.enhancer.EnhanceServiceImpl;
import io.github.bootystar.mybatisplus.generator.config.child.EnhanceConfig;

/**
 * 继承实现型代码生成器
 *
 * @author booty
 */
public class EnhanceGenerator extends AbstractGenerator{

    protected EnhanceConfig.Builder customConfigBuilder = new EnhanceConfig.Builder();

    public EnhanceConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }
    
    public EnhanceGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();
//        // 自3.5.6废弃
//        templateConfigBuilder
//                .service("/parent/service.java")
//            .serviceImpl("/parent/serviceImpl.java")
//            .mapper("/parent/mapper.java")
//        ;
        strategyConfigBuilder.mapperBuilder()
                .superClass(EnhanceMapper.class)
                .mapperTemplate("/parent/mapper.java");
        ;
        
        strategyConfigBuilder.serviceBuilder()
                    .superServiceClass(EnhanceService.class)
                    .superServiceImplClass(EnhanceServiceImpl.class)
                .serviceImplTemplate("/parent/serviceImpl.java")
                .serviceTemplate("/parent/service.java");
        ;
     
                
    }

}
