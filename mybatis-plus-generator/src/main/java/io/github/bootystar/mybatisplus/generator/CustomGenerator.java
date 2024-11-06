package io.github.bootystar.mybatisplus.generator;


import io.github.bootystar.mybatisplus.base.IBaseService;
import io.github.bootystar.mybatisplus.base.impl.CustomServiceImpl;
import io.github.bootystar.mybatisplus.base.mapper.CustomMapper;
import io.github.bootystar.mybatisplus.config.CustomConfig;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * 自写mapper实现生成器
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
        strategyConfigBuilder.mapperBuilder()
                .superClass(CustomMapper.class)
                .mapperTemplate("/custom/mapper.java")
                .mapperXmlTemplate("/common/mapper.xml")
        ;
        
        strategyConfigBuilder.serviceBuilder()
                    .superServiceClass(IBaseService.class)
                    .superServiceImplClass(CustomServiceImpl.class)
                .serviceImplTemplate("/custom/serviceImpl.java")
                .serviceTemplate("/common/service.java");
        ;
     
                
    }

}
