package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.base.GenericMapper;
import io.github.bootystar.mybatisplus.base.GenericService;
import io.github.bootystar.mybatisplus.base.impl.SplicingServiceImpl;
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
        ;

        strategyConfigBuilder.serviceBuilder()
                .superServiceClass(GenericService.class)
                .superServiceImplClass(SplicingServiceImpl.class)
                .serviceTemplate("/common/serviceG.java")
                .serviceImplTemplate("/common/serviceImplG.java")
        ;

        strategyConfigBuilder.mapperBuilder()
                .superClass(GenericMapper.class)
                .mapperTemplate("/common/mapperG.java")
                .mapperXmlTemplate("/splicing/mapper.xml")
        ;
        // todo 临时必要项, 后续优化
        strategyConfigBuilder.entityBuilder()
                .enableTableFieldAnnotation();

    }

}
