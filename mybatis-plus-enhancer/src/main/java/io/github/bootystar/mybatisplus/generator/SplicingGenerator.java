package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.core.GenericMapper;
import io.github.bootystar.mybatisplus.core.GenericService;
import io.github.bootystar.mybatisplus.core.impl.SplicingServiceImpl;
import io.github.bootystar.mybatisplus.config.SplicingConfig;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * SQL拼接实现生成器
 *
 * @author bootystar
 */
public class SplicingGenerator extends AbstractGenerator {

    protected SplicingConfig.Builder customConfigBuilder = new SplicingConfig.Builder();

    public SplicingConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }

    public SplicingGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();

        templateConfigBuilder
                .service("/common/serviceG.java")
                .serviceImpl("/common/serviceImplG.java")
                .mapper("/common/mapperG.java")
                .xml("/splicing/mapper.xml")
        ;

        strategyConfigBuilder.serviceBuilder()
                .superServiceClass(GenericService.class)
                .superServiceImplClass(SplicingServiceImpl.class)
        ;

        strategyConfigBuilder.mapperBuilder()
                .superClass(GenericMapper.class)
        ;

    }

}
