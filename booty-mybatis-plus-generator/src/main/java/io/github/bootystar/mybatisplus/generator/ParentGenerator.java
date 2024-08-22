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
        templateConfigBuilder.service("/parent/service.java");
        templateConfigBuilder.serviceImpl("/parent/serviceImpl.java");
        templateConfigBuilder.mapper("/parent/mapper.java");
        strategyConfigBuilder.mapperBuilder()
                .superClass(CustomMapper.class)
        ;
        strategyConfigBuilder.serviceBuilder()
                    .superServiceClass(CustomService.class)
                    .superServiceImplClass(CustomServiceImpl.class)
        ;
    }

}
