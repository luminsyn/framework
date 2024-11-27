package io.github.bootystar.mybatisplus.generator.core.impl;


import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.impl.EnhanceServiceImpl;
import io.github.bootystar.mybatisplus.generator.config.impl.EnhanceConfig;
import io.github.bootystar.mybatisplus.generator.core.base.AbstractGenerator;

/**
 * 额外属性型生成器
 * 生成可供查询的额外属性
 *
 * @author booty
 */
public class ExtraParamGenerator extends AbstractGenerator<EnhanceConfig, EnhanceConfig.Builder> {

    public ExtraParamGenerator(String url, String username, String password) {
        super(url, username, password, new EnhanceConfig.Builder());
    }

    @Override
    protected void config4child() {
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/templates/enhance/service.java")
                .serviceImplTemplate("/templates/enhance/serviceImpl.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/templates/enhance/mapper.java")
        ;
        strategyConfigBuilder.serviceBuilder()
                .superServiceClass(EnhanceService.class)
                .superServiceImplClass(EnhanceServiceImpl.class)
        ;
        strategyConfigBuilder.mapperBuilder()
                .superClass(EnhanceMapper.class)
        ;
    }

}
