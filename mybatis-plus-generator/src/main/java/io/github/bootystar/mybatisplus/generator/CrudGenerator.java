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
 * @author booty
 * @since 2023/7/13 15:34
 * @see  ConstVal;
 */

public class CrudGenerator {

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

    public CrudGenerator(String url, String username, String password) {
        this.dataSourceConfigBuilder = new DataSourceConfig.Builder(url, username, password);
        init();
    }


    protected void init() {
        String projectPath = System.getProperty("user.dir");
        globalConfigBuilder.author("booty").disableOpenDir().outputDir( projectPath+ "/src/main/java")
        ;
        packageConfigBuilder.parent("io.github.bootystar")
        ;
        strategyConfigBuilder.controllerBuilder().enableRestStyle()
        ;
        strategyConfigBuilder.mapperBuilder().mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
        ;
        strategyConfigBuilder.serviceBuilder()
        ;

        templateConfigBuilder.controller("/crud/controller.java");
        templateConfigBuilder.service("/crud/service.java");
        templateConfigBuilder.serviceImpl("/crud/serviceImpl.java");
        templateConfigBuilder.mapper("/crud/mapper.java");
        templateConfigBuilder.xml("/crud/mapper.xml");
        templateConfigBuilder.entity("/crud/entity.java");
        customConfigBuilder.returnResultClass(Result.class);
        customConfigBuilder.returnResultGenericType(true);
        customConfigBuilder.returnResultDefaultStaticMethodName("success");
        customConfigBuilder.insertExcludeFields(Arrays.asList("createTime","updateTime"));
        customConfigBuilder.updateExcludeFields(Arrays.asList("createTime","updateTime"));
        customConfigBuilder.orderColumn("create_time",true);
        customConfigBuilder.orderColumn("id",true);
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

        String DTOPackage = customConfig.getDTOPackage().replaceAll("\\.", "\\" + File.separator);
        String VOPackage = customConfig.getVOPackage().replaceAll("\\.", "\\" + File.separator);
        LinkedList<CustomFile> customFiles = new LinkedList<>();


        if (customConfig.isGenerateInsert()){
            CustomFile InsertDto = new CustomFile.Builder().fileName("InsertDTO.java").templatePath("/crud/entityInsertDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(InsertDto);
        }
        if (customConfig.isGenerateUpdate()){
            CustomFile updateDto = new CustomFile.Builder().fileName("UpdateDTO.java").templatePath("/crud/entityUpdateDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(updateDto);
        }
       if (customConfig.isGenerateSelect()){
           CustomFile selectDto = new CustomFile.Builder().fileName("SelectDTO.java").templatePath("/crud/entitySelectDTO.java.vm").packageName(DTOPackage).build();
           customFiles.add(selectDto);
       }
       if (customConfig.isGenerateExport() && !customConfig.isExportOnVO()){
           CustomFile exportDto = new CustomFile.Builder().fileName("ExportDTO.java").templatePath("/crud/entityExportDTO.java.vm").packageName(DTOPackage).build();
           customFiles.add(exportDto);
       }

        if (customConfig.isGenerateImport() && !customConfig.isImportOnVO()){
            CustomFile importDto = new CustomFile.Builder().fileName("ImportDTO.java").templatePath("/crud/entityImportDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(importDto);
        }

        CustomFile vo = new CustomFile.Builder().fileName("VO.java").templatePath("/crud/entityVO.java.vm").packageName(VOPackage).build();
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
