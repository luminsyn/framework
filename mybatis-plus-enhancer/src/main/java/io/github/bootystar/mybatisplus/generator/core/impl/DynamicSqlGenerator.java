package io.github.bootystar.mybatisplus.generator.core.impl;

import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.impl.DynamicSqlServiceImpl;
import io.github.bootystar.mybatisplus.generator.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generator.core.base.AbstractGenerator;

/**
 * 动态sql型生成器
 *
 * @author bootystar
 */
public class DynamicSqlGenerator extends AbstractGenerator<DynamicSqlConfig, DynamicSqlConfig.Builder> {

    public DynamicSqlGenerator(String url, String username, String password) {
        super(url, username, password, new DynamicSqlConfig.Builder());
    }

    @Override
    protected void config4child() {
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/templates/enhance/service.java")
                .serviceImplTemplate("/templates/enhance/serviceImpl.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/templates/enhance/mapper.java")
                .mapperXmlTemplate("/templates/dynamic/mapper.xml")
        ;
        strategyConfigBuilder.serviceBuilder()
                .superServiceClass(EnhanceService.class)
                .superServiceImplClass(DynamicSqlServiceImpl.class)
        ;
        strategyConfigBuilder.mapperBuilder()
                .superClass(EnhanceMapper.class)
        ;
    }

}
