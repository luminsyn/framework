package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.base.mapper.InjectionMapper;
import io.github.bootystar.mybatisplus.base.IBaseService;
import io.github.bootystar.mybatisplus.base.impl.InjectionServiceImpl;
import io.github.bootystar.mybatisplus.config.InjectionConfig;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * 注入器实现生成器
 *
 * @author bootystar
 */
public class InjectionGenerator extends AbstractGenerator {

    protected InjectionConfig.Builder customConfigBuilder = new InjectionConfig.Builder();

    public InjectionConfig.Builder customConfigBuilder() {
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
                    .superServiceClass(IBaseService.class)
                    .superServiceImplClass(InjectionServiceImpl.class)
                .serviceImplTemplate("/injection/serviceImpl.java")
                .serviceTemplate("/common/service.java");
        ;
     
                
    }

}
