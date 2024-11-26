package io.github.bootystar.mybatisplus.generator.core.impl;


import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.impl.ExtraFieldServiceImpl;
import io.github.bootystar.mybatisplus.generator.config.impl.ExtraFieldConfig;
import io.github.bootystar.mybatisplus.generator.core.base.AbstractGenerator;

/**
 * 额外属性型生成器
 * 生成可供查询的额外属性
 *
 * @author booty
 */
public class ExtraFieldGenerator extends AbstractGenerator<ExtraFieldConfig, ExtraFieldConfig.Builder> {

    public ExtraFieldGenerator(String url, String username, String password) {
        super(url, username, password, new ExtraFieldConfig.Builder());
    }

    @Override
    protected void config4child() {
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/velocityTemplates/service.enhance.java" )
                .serviceImplTemplate("/velocityTemplates/serviceImpl.enhance.java" )
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/velocityTemplates/mapper.enhance.java" )
        ;
        strategyConfigBuilder.serviceBuilder()
                .superServiceClass(EnhanceService.class)
                .superServiceImplClass(ExtraFieldServiceImpl.class)
        ;
        strategyConfigBuilder.mapperBuilder()
                .superClass(EnhanceMapper.class)
        ;
    }

}
