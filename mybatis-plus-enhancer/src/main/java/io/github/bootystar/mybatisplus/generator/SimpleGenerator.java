package io.github.bootystar.mybatisplus.generator;

import io.github.bootystar.mybatisplus.config.SimpleConfig;
import io.github.bootystar.mybatisplus.generator.base.AbstractGenerator;

/**
 * 代码注入生成器
 *
 * @author bootystar
 */
public class SimpleGenerator extends AbstractGenerator {
    protected SimpleConfig.Builder customConfigBuilder = new SimpleConfig.Builder();
    public SimpleConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }

    public SimpleGenerator(String url, String username, String password) {
        super(url, username, password);
        super.init();
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/simple/service.java")
                .serviceImplTemplate("/simple/serviceImpl.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/simple/mapper.java")
                .mapperXmlTemplate("/simple/mapper.xml")
        ;
    }


}
