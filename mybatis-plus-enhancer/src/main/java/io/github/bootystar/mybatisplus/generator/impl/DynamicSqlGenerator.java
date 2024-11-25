package io.github.bootystar.mybatisplus.generator.impl;

import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.impl.DynamicSqlServiceImpl;
import io.github.bootystar.mybatisplus.generator.base.GeneratorBase;
import io.github.bootystar.mybatisplus.generator.config.impl.DynamicSqlConfig;
import io.github.bootystar.mybatisplus.generator.config.impl.SubClassConfig;

/**
 * 动态sql型生成器
 *
 * @author bootystar
 */
public class DynamicSqlGenerator extends GeneratorBase<DynamicSqlConfig.Builder> {

    public DynamicSqlGenerator(String url, String username, String password) {
        super(url, username, password, new DynamicSqlConfig.Builder());
    }

    @Override
    protected void config4child() {
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/common/service.enhance.java")
                .serviceImplTemplate("/common/service.enhance.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/common/mapper.enhance.java")
                .mapperXmlTemplate("/common/mapperXml.dynamic.xml")
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
