package io.github.bootystar.mybatisplus.generator;


import io.github.bootystar.mybatisplus.config.CustomConfig;
import io.github.bootystar.mybatisplus.core.GenericMapper;
import io.github.bootystar.mybatisplus.core.GenericService;
import io.github.bootystar.mybatisplus.core.impl.CustomServiceImpl;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * 自写mapper继承实现生成器
 *
 * @author booty
 */
public class CustomGenerator extends AbstractGenerator<CustomConfig.Builder> {

    public CustomGenerator(String url, String username, String password) {
        super(url, username, password, new CustomConfig.Builder());
    }

    @Override
    protected void config4child() {
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/common/serviceG.java")
                .serviceImplTemplate("/common/serviceImplG.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/common/mapperG.java")
        ;

        strategyConfigBuilder.serviceBuilder()
                .superServiceClass(GenericService.class)
                .superServiceImplClass(CustomServiceImpl.class)
        ;

        strategyConfigBuilder.mapperBuilder()
                .superClass(GenericMapper.class)
        ;
    }

}
