package io.github.bootystar.mybatisplus.generator;

import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import io.github.bootystar.mybatisplus.generator.config.ParentConfig;
import io.github.bootystar.mybatisplus.generator.core.CustomMapper;
import io.github.bootystar.mybatisplus.generator.core.CustomService;
import io.github.bootystar.mybatisplus.generator.core.CustomServiceImpl;
import io.github.bootystar.mybatisplus.generator.core.Result;
import lombok.Getter;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedList;

/**
 * @Author booty
 * @Date 2023/7/13 15:34
 * @see  ConstVal;
 */

public class ParentGenerator  {

    protected DataSourceConfig.Builder dataSourceConfigBuilder ;

    protected GlobalConfig.Builder globalConfigBuilder = new GlobalConfig.Builder();

    protected PackageConfig.Builder packageConfigBuilder =new PackageConfig.Builder();

    protected StrategyConfig.Builder strategyConfigBuilder=new  StrategyConfig.Builder();

    protected InjectionConfig.Builder injectionConfigBuilder=new InjectionConfig.Builder();

    protected TemplateConfig.Builder templateConfigBuilder = new TemplateConfig.Builder();
    @Getter
    protected ParentConfig.Builder customConfigBuilder = new ParentConfig.Builder();


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
    public ParentConfig.Builder customConfigBuilder() {
        return customConfigBuilder;
    }

    public ParentGenerator(String url, String username, String password) {
        this.dataSourceConfigBuilder = new DataSourceConfig.Builder(url, username, password);
        init();
    }


    protected void init() {
        String projectPath = System.getProperty("user.dir");
        globalConfigBuilder.author("booty").disableOpenDir().outputDir( projectPath+ "/src/main/java")
        ;
        packageConfigBuilder.parent("io.github.bootystar")
        ;
        strategyConfigBuilder.controllerBuilder().enableRestStyle();
        strategyConfigBuilder.mapperBuilder().mapperAnnotation(org.apache.ibatis.annotations.Mapper.class).superClass(CustomMapper.class);
        strategyConfigBuilder.serviceBuilder().superServiceClass(CustomService.class).superServiceImplClass(CustomServiceImpl.class)
        ;

        templateConfigBuilder.controller("/parent/controller.java");
        templateConfigBuilder.service("/parent/service.java");
        templateConfigBuilder.serviceImpl("/parent/serviceImpl.java");
        templateConfigBuilder.mapper("/parent/mapper.java");
        templateConfigBuilder.xml("/parent/mapper.xml");
        templateConfigBuilder.entity("/parent/entity.java");
        customConfigBuilder.returnResultClass(Result.class);
        customConfigBuilder.returnResultGenericType(true);
        customConfigBuilder.returnResultDefaultStaticMethodName("success");
        customConfigBuilder.insertExcludeFields(Arrays.asList("createTime","updateTime","version"));
        customConfigBuilder.updateExcludeFields(Arrays.asList("createTime","updateTime"));
        customConfigBuilder.orderColumn("create_time",true);
        customConfigBuilder.requestBody(true);
        customConfigBuilder.enableValidated(true);
        customConfigBuilder.generateInsert(true);
        customConfigBuilder.generateUpdate(true);
        customConfigBuilder.generateSelect(true);
        customConfigBuilder.generateExport(true);
        customConfigBuilder.generateImport(true);
        customConfigBuilder.serviceImplOverride(false);
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


        ParentConfig customConfig=customConfigBuilder.build();

        String dtoPackage = customConfig.getDtoPackage().replaceAll("\\.", "\\" + File.separator);
        String voPackage = customConfig.getVoPackage().replaceAll("\\.", "\\" + File.separator);
        LinkedList<CustomFile> customFiles = new LinkedList<>();


        if (customConfig.getGenerateInsert()){
            CustomFile InsertDto = new CustomFile.Builder().fileName("InsertDto.java").templatePath("/parent/entityInsertDto.java.vm").packageName(dtoPackage).build();
            customFiles.add(InsertDto);
        }
        if (customConfig.getGenerateUpdate()){
            CustomFile updateDto = new CustomFile.Builder().fileName("UpdateDto.java").templatePath("/parent/entityUpdateDto.java.vm").packageName(dtoPackage).build();
            customFiles.add(updateDto);
        }
       if (customConfig.getGenerateSelect()){
           CustomFile selectDto = new CustomFile.Builder().fileName("SelectDto.java").templatePath("/parent/entitySelectDto.java.vm").packageName(dtoPackage).build();
           customFiles.add(selectDto);
       }
       if (customConfig.getGenerateExport()){
           CustomFile exportDto = new CustomFile.Builder().fileName("ExportDto.java").templatePath("/parent/entityExportDto.java.vm").packageName(dtoPackage).build();
           customFiles.add(exportDto);
       }

        if (customConfig.getGenerateImport()){
            CustomFile importDto = new CustomFile.Builder().fileName("ImportDto.java").templatePath("/parent/entityImportDto.java.vm").packageName(dtoPackage).build();
            customFiles.add(importDto);
        }

        CustomFile vo = new CustomFile.Builder().fileName("Vo.java").templatePath("/parent/entityVo.java.vm").packageName(voPackage).build();
        customFiles.add(vo);



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
