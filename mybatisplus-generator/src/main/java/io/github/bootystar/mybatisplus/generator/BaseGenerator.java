package io.github.bootystar.mybatisplus.generator;

import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.*;
import io.github.bootystar.mybatisplus.generator.config.CustomConfig;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @Author booty
 * @Date 2023/7/13 14:19
 * @see  com.baomidou.mybatisplus.generator.config.ConstVal;
 */
public class BaseGenerator {

    protected DataSourceConfig.Builder dataSourceConfigBuilder ;

    protected GlobalConfig.Builder globalConfigBuilder = new GlobalConfig.Builder();

    protected PackageConfig.Builder packageConfigBuilder =new PackageConfig.Builder();

    protected StrategyConfig.Builder strategyConfigBuilder=new  StrategyConfig.Builder();

    protected InjectionConfig.Builder injectionConfigBuilder=new InjectionConfig.Builder();

    protected TemplateConfig.Builder templateConfigBuilder = new TemplateConfig.Builder();

    protected CustomConfig.Builder customConfigBuilder = new CustomConfig.Builder();


    /**
     * 不指定表名,
     *
     * @author booty
     * @date 2023/07/13 14:40
     */
    public void execute(){
        CustomGenerator customGenerator =
                new CustomGenerator(dataSourceConfigBuilder.build())
                        .global(globalConfigBuilder.build())
                        // 包配置
                        .packageInfo(packageConfigBuilder.build())
                        // 策略配置
                        .strategy(strategyConfigBuilder.build())
                        // 注入配置
                        .injection(injectionConfigBuilder.build())
                        // 模板配置
                        .template(templateConfigBuilder.build())
                        // 自定义配置
                        .custom(customConfigBuilder.build())
                ;
        customGenerator.execute();
    }


    /**
     * 执行
     *
     * @param tableNames 表名
     * @author booty
     * @date 2023/07/13 14:40
     */
    public void execute(String... tableNames){
        strategyConfigBuilder.addInclude(Arrays.asList(tableNames));
        execute();
    }


    public BaseGenerator(String url, String username, String password) {
        this.dataSourceConfigBuilder = new DataSourceConfig.Builder(url, username, password);
        init();
    }

    /**
     * 初始化
     *
     * @author booty
     * @date 2023/07/13 14:42
     */
    protected void init(){
        String projectPath = System.getProperty("user.dir");

        // 全局设置
        globalConfigBuilder
                .author("booty")
                .disableOpenDir()    //禁止打开输出目录
                .outputDir( projectPath+ "/src/main/java")  // 指定输出目录
        ;
        // 包名设置
        packageConfigBuilder
                .parent("io.github.bootystar")
//                .pathInfo(Collections.singletonMap(OutputFile.xml, projectPath + "/src/main/resources"))
        ;
        // 策略设置
        strategyConfigBuilder.controllerBuilder()

        ;

        strategyConfigBuilder.serviceBuilder()

        ;

        strategyConfigBuilder.mapperBuilder()
                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class);

        strategyConfigBuilder.entityBuilder()

        ;
    }


    public DataSourceConfig.Builder dataSourceConfigBuilder() {
        return dataSourceConfigBuilder;
    }

    public GlobalConfig.Builder globalConfigBuilder() {
        return globalConfigBuilder;
    }

    public PackageConfig.Builder packageConfigBuilder() {
        return packageConfigBuilder;
    }

    public StrategyConfig.Builder strategyConfigBuilder() {
        return strategyConfigBuilder;
    }

    public InjectionConfig.Builder injectionConfigBuilder() {
        return injectionConfigBuilder;
    }

    public TemplateConfig.Builder templateConfigBuilder() {
        return templateConfigBuilder;
    }

    public CustomConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }
}
