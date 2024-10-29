package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.core.CustomMapper;
import io.github.bootystar.mybatisplus.core.CustomService;
import io.github.bootystar.mybatisplus.core.CustomServiceImpl;
import io.github.bootystar.mybatisplus.generator.config.child.ParentConfig;

/**
 * 继承实现型代码生成器
 *
 * @author booty
 */
public class ParentGenerator extends AbstractGenerator{

    protected ParentConfig.Builder customConfigBuilder = new ParentConfig.Builder();

    public ParentConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }
    
    public ParentGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();
//        // 自3.5.6废弃
//        templateConfigBuilder
//                .service("/parent/service.java")
//            .serviceImpl("/parent/serviceImpl.java")
//            .mapper("/parent/mapper.java")
//        ;
        strategyConfigBuilder.mapperBuilder()
                .superClass(CustomMapper.class)
                .mapperTemplate("/parent/mapper.java");
        ;
        
        strategyConfigBuilder.serviceBuilder()
                    .superServiceClass(CustomService.class)
                    .superServiceImplClass(CustomServiceImpl.class)
                .serviceImplTemplate("/parent/serviceImpl.java")
                .serviceTemplate("/parent/service.java");
        ;
     
                
    }

}
