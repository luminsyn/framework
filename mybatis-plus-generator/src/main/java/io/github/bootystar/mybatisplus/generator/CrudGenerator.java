package io.github.bootystar.mybatisplus.generator;

import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import io.github.bootystar.mybatisplus.generator.config.CrudConfig;
import io.github.bootystar.mybatisplus.generator.core.Result;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedList;

/**
 * @author booty
 * @since 2023/7/13 15:34
 * @see  com.baomidou.mybatisplus.generator.config.ConstVal;
 */

public class CrudGenerator{

    protected DataSourceConfig.Builder dataSourceConfigBuilder ;

    protected GlobalConfig.Builder globalConfigBuilder = new GlobalConfig.Builder();

    protected PackageConfig.Builder packageConfigBuilder =new PackageConfig.Builder();

    protected StrategyConfig.Builder strategyConfigBuilder=new  StrategyConfig.Builder();

    protected InjectionConfig.Builder injectionConfigBuilder=new InjectionConfig.Builder();

    protected TemplateConfig.Builder templateConfigBuilder = new TemplateConfig.Builder();
    protected CrudConfig.Builder customConfigBuilder = new CrudConfig.Builder();


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

    public CrudConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }


    public CrudGenerator(String url, String username, String password) {
        this.dataSourceConfigBuilder = new DataSourceConfig.Builder(url, username, password);
        init();
    }

    protected void init() {
        String projectPath = System.getProperty("user.dir");
        // 全局设置
        globalConfigBuilder.author("booty").disableOpenDir().outputDir( projectPath+ "/src/main/java")
        ;
        // 包名设置
        packageConfigBuilder.parent("io.github.bootystar")
        ;
        // 策略设置
        strategyConfigBuilder.controllerBuilder()
        ;

        strategyConfigBuilder.serviceBuilder()
        ;

        strategyConfigBuilder.mapperBuilder().mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
        ;

        strategyConfigBuilder.entityBuilder()
        ;
        strategyConfigBuilder.controllerBuilder().enableRestStyle();
        templateConfigBuilder.controller("/crud/controller.java");
        templateConfigBuilder.service("/crud/service.java");
        templateConfigBuilder.serviceImpl("/crud/serviceImpl.java");
        templateConfigBuilder.mapper("/crud/mapper.java");
        templateConfigBuilder.xml("/crud/mapper.xml");
        templateConfigBuilder.entity("/crud/entity.java");
        customConfigBuilder.returnResultClass(Result.class);
        customConfigBuilder.returnResultGenericType(true);
        customConfigBuilder.returnResultDefaultStaticMethodName("success");
        customConfigBuilder.pageByDto(true);
        customConfigBuilder.exportExcel(true);
        customConfigBuilder.importExcel(true);
        customConfigBuilder.insertExcludeFields(Arrays.asList("createTime","updateTime","version"));
        customConfigBuilder.updateExcludeFields(Arrays.asList("createTime","updateTime"));
        customConfigBuilder.orderColumn("create_time",true);
        customConfigBuilder.requestBody(true);
        customConfigBuilder.enableValidated(true);
    }


    public void execute(String... tableNames){
        strategyConfigBuilder.addInclude(Arrays.asList(tableNames));
        execute();
    }

    public void execute() {

        DataSourceConfig dataSourceConfig = dataSourceConfigBuilder.build();
        GlobalConfig globalConfig = globalConfigBuilder.build();
        StrategyConfig strategyConfig = strategyConfigBuilder.build();
        TemplateConfig templateConfig = templateConfigBuilder.build();
        InjectionConfig injectionConfig = injectionConfigBuilder.build();
        PackageConfig packageConfig = packageConfigBuilder.build();
        CrudConfig customConfig = customConfigBuilder.build();

        String dtoPackage = customConfig.getDtoPackage().replaceAll("\\.", "\\" + File.separator);
        String voPackage = customConfig.getVoPackage().replaceAll("\\.", "\\" + File.separator);
        String listenerPackage = customConfig.getListenerPackage().replaceAll("\\.", "\\" + File.separator);

        LinkedList<CustomFile> customFiles = new LinkedList<>();
        CustomFile InsertDto = new CustomFile.Builder().fileName("InsertDto.java").templatePath("/crud/entityInsertDto.java.vm").packageName(dtoPackage).build();
        customFiles.add(InsertDto);
        CustomFile updateDto = new CustomFile.Builder().fileName("UpdateDto.java").templatePath("/crud/entityUpdateDto.java.vm").packageName(dtoPackage).build();
        customFiles.add(updateDto);
        if (customConfig.getPageByDto()){
            CustomFile selectDto = new CustomFile.Builder().fileName("SelectDto.java").templatePath("/crud/entitySelectDto.java.vm").packageName(dtoPackage).build();
            customFiles.add(selectDto);
            CustomFile vo = new CustomFile.Builder().fileName("Vo.java").templatePath("/crud/entityVo.java.vm").packageName(voPackage).build();
            customFiles.add(vo);
            if (customConfig.getExportExcel()){
                CustomFile exportVo = new CustomFile.Builder().fileName("ExportVo.java").templatePath("/crud/entityExportVo.java.vm").packageName(voPackage).build();
                customFiles.add(exportVo);
            }
        }
        if (customConfig.getImportExcel()){
            CustomFile importVo = new CustomFile.Builder().fileName("ImportVo.java").templatePath("/crud/entityImportVo.java.vm").packageName(voPackage).build();
            CustomFile listener = new CustomFile.Builder().fileName("Listener.java").templatePath("/crud/listener.java.vm").packageName(listenerPackage).build();
            customFiles.add(importVo);
            customFiles.add(listener);
        }
        customConfig.setCustomFiles(customFiles);

        CustomGenerator customGenerator =
                new CustomGenerator(dataSourceConfig)
                        .global(globalConfig)
                        // 包配置
                        .packageInfo(packageConfig)
                        // 策略配置
                        .strategy(strategyConfig)
                        // 模板配置
                        .template(templateConfig)
                        // 注入配置
                        .injection(injectionConfig)
                        // 自定义配置
                        .custom(customConfig)
        ;

        customGenerator.execute();
    }
}
