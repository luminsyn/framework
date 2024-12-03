package io.github.bootystar.mybatisplus.generate.generator.impl;


import io.github.bootystar.mybatisplus.enhance.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.core.impl.DynamicFieldServiceImpl;
import io.github.bootystar.mybatisplus.generate.config.impl.DynamicFieldConfig;
import io.github.bootystar.mybatisplus.generate.generator.core.AbstractGenerator;

/**
 * 额外属性型生成器
 * 生成可供查询的额外属性
 *
 * @author booty
 */
public class DynamicFieldGenerator extends AbstractGenerator<DynamicFieldConfig, DynamicFieldConfig.Builder> {

    public DynamicFieldGenerator(String url, String username, String password) {
        super(url, username, password, new DynamicFieldConfig.Builder());
        this.strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/templates/enhance/service.java")
                .serviceImplTemplate("/templates/extra/serviceImpl.java")
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/templates/enhance/mapper.java")
                .mapperXmlTemplate("/templates/enhance/mapper.xml")
        ;
        this.strategyConfigBuilder.serviceBuilder()
                .superServiceClass(DynamicService.class)
                .superServiceImplClass(DynamicFieldServiceImpl.class)
        ;
        this.strategyConfigBuilder.mapperBuilder()
                .superClass(DynamicMapper.class)
        ;
    }

}
