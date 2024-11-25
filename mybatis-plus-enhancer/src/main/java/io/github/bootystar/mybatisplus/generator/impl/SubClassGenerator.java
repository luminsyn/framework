package io.github.bootystar.mybatisplus.generator.impl;


import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.impl.ExtraFieldServiceImpl;
import io.github.bootystar.mybatisplus.generator.base.GeneratorBase;
import io.github.bootystar.mybatisplus.generator.config.impl.SubClassConfig;

/**
 * 额外属性型生成器
 * 生成可供查询的额外属性
 *
 * @author booty
 */
public class SubClassGenerator extends GeneratorBase<SubClassConfig.Builder> {

    public SubClassGenerator(String url, String username, String password) {
        super(url, username, password, new SubClassConfig.Builder());
    }

    @Override
    protected void config4child() {
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/common/service.enhance.java")
                .serviceImplTemplate("/common/serviceImpl.enhance.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/common/mapper.enhance.java")
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
