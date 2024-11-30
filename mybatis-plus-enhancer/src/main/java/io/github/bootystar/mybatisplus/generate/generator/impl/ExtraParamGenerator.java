package io.github.bootystar.mybatisplus.generate.generator.impl;


import io.github.bootystar.mybatisplus.enhance.EnhanceMapper;
import io.github.bootystar.mybatisplus.enhance.EnhanceService;
import io.github.bootystar.mybatisplus.enhance.impl.ExtraFieldServiceImpl;
import io.github.bootystar.mybatisplus.generate.config.impl.ExtraFiledConfig;
import io.github.bootystar.mybatisplus.generate.generator.core.AbstractGenerator;

/**
 * 额外属性型生成器
 * 生成可供查询的额外属性
 *
 * @author booty
 */
public class ExtraParamGenerator extends AbstractGenerator<ExtraFiledConfig, ExtraFiledConfig.Builder> {

    public ExtraParamGenerator(String url, String username, String password) {
        super(url, username, password, new ExtraFiledConfig.Builder());
        this.strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/templates/enhance/service.java")
                .serviceImplTemplate("/templates/enhance/serviceImpl.java")
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/templates/enhance/mapper.java")
        ;
        this.strategyConfigBuilder.serviceBuilder()
                .superServiceClass(EnhanceService.class)
                .superServiceImplClass(ExtraFieldServiceImpl.class)
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .superClass(EnhanceMapper.class)
        ;
    }

}
