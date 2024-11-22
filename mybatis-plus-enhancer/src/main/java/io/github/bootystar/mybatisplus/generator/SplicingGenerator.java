package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.config.SimpleConfig;
import io.github.bootystar.mybatisplus.config.SplicingConfig;
import io.github.bootystar.mybatisplus.core.GenericMapper;
import io.github.bootystar.mybatisplus.core.GenericService;
import io.github.bootystar.mybatisplus.core.impl.SplicingServiceImpl;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * SQL拼接实现生成器
 *
 * @author bootystar
 */
public class SplicingGenerator extends AbstractGenerator<SplicingConfig, SplicingConfig.Builder> {

    public SplicingGenerator(String url, String username, String password) {
        super(url, username, password, new SplicingConfig.Builder());
    }

    @Override
    protected void config4child() {
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/common/serviceG.java")
                .serviceImplTemplate("/common/serviceImplG.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/common/mapperG.java")
                .mapperXmlTemplate("/splicing/mapper.xml")
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
