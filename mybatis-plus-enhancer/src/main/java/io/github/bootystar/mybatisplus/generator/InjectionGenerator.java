package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.base.injection.InjectionMapper;
import io.github.bootystar.mybatisplus.base.injection.InjectionService;
import io.github.bootystar.mybatisplus.base.injection.InjectionServiceImpl;
import io.github.bootystar.mybatisplus.config.EnhanceConfig;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * 注入器实现生成器
 *
 * @author bootystar
 */
public class InjectionGenerator extends AbstractGenerator {

    protected EnhanceConfig.Builder customConfigBuilder = new EnhanceConfig.Builder();

    public EnhanceConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }
    
    public InjectionGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();
        strategyConfigBuilder.mapperBuilder()
                .superClass(InjectionMapper.class)
                .mapperTemplate("/injection/mapper.java")
                .mapperXmlTemplate("/injection/mapper.xml")

        ;
        strategyConfigBuilder.serviceBuilder()
                    .superServiceClass(InjectionService.class)
                    .superServiceImplClass(InjectionServiceImpl.class)
                .serviceImplTemplate("/injection/serviceImpl.java")
                .serviceTemplate("/injection/service.java");
        ;
     
                
    }

}
