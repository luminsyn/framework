package io.github.bootystar.mybatisplus.generator;


import io.github.bootystar.mybatisplus.core.GenericMapper;
import io.github.bootystar.mybatisplus.core.GenericService;
import io.github.bootystar.mybatisplus.core.impl.CustomServiceImpl;
import io.github.bootystar.mybatisplus.config.CustomConfig;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * 自写mapper继承实现生成器
 *
 * @author booty
 */
public class CustomGenerator extends AbstractGenerator {

    protected CustomConfig.Builder customConfigBuilder = new CustomConfig.Builder();

    public CustomConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }

    public CustomGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();
        templateConfigBuilder
                .service("/common/serviceG.java")
                .serviceImpl("/common/serviceImplG.java")
                .mapper("/common/mapperG.java")
        ;


        strategyConfigBuilder.serviceBuilder()
                .superServiceClass(GenericService.class)
                .superServiceImplClass(CustomServiceImpl.class)
                .serviceTemplate("/common/serviceG.java")
                .serviceImplTemplate("/common/serviceImplG.java")
        ;

        strategyConfigBuilder.mapperBuilder()
                .superClass(GenericMapper.class)
                .mapperTemplate("/common/mapperG.java")
        ;

    }

}
