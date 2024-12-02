package io.github.bootystar.mybatisplus.generate.generator.impl;

import io.github.bootystar.mybatisplus.enhance.EnhanceMapper;
import io.github.bootystar.mybatisplus.enhance.EnhanceService;
import io.github.bootystar.mybatisplus.enhance.impl.DynamicSqlServiceImpl;
import io.github.bootystar.mybatisplus.generate.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generate.generator.core.AbstractGenerator;

/**
 * 动态sql型生成器
 *
 * @author bootystar
 */
public class DynamicSqlGenerator extends AbstractGenerator<DynamicSqlConfig, DynamicSqlConfig.Builder> {

    public DynamicSqlGenerator(String url, String username, String password) {
        super(url, username, password, new DynamicSqlConfig.Builder());
        this.strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/templates/enhance/service.java")
                .serviceImplTemplate("/templates/enhance/serviceImpl.java")
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/templates/enhance/mapper.java")
                .mapperXmlTemplate("/templates/enhance/mapper.xml")
        ;
        this.strategyConfigBuilder.serviceBuilder()
                .superServiceClass(EnhanceService.class)
                .superServiceImplClass(DynamicSqlServiceImpl.class)
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .superClass(EnhanceMapper.class)
        ;
    }

}
