package io.github.bootystar.mybatisplus.generator.base;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.rules.DateType;
import com.baomidou.mybatisplus.generator.config.rules.DbColumnType;
import io.github.bootystar.mybatisplus.config.SplicingConfig;
import io.github.bootystar.mybatisplus.config.base.ConfigBase;
import io.github.bootystar.mybatisplus.config.base.ConfigBaseBuilder;
import io.github.bootystar.mybatisplus.config.base.IConfig;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.JdbcType;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;

/**
 * 自定义配置代码生成器-抽象父类
 *
 * @author bootystar
 */
@Slf4j
@SuppressWarnings("unused")
public abstract class AbstractGenerator<C extends ConfigBase, B extends ConfigBaseBuilder<C, B>> {

    protected DataSourceConfig.Builder dataSourceConfigBuilder;

    protected GlobalConfig.Builder globalConfigBuilder = new GlobalConfig.Builder();

    protected PackageConfig.Builder packageConfigBuilder = new PackageConfig.Builder();

    protected StrategyConfig.Builder strategyConfigBuilder = new StrategyConfig.Builder();

    protected InjectionConfig.Builder injectionConfigBuilder = new InjectionConfig.Builder();

//    protected TemplateConfig.Builder templateConfigBuilder = new TemplateConfig.Builder();

    protected ConfigBaseBuilder<C, B> customConfigBuilder;

    public ConfigBaseBuilder<C, B> customConfigBuilder() {
        return customConfigBuilder;
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

//    public TemplateConfig.Builder templateConfigBuilder() {
//        return templateConfigBuilder;
//    }

    public InjectionConfig.Builder injectionConfigBuilder() {
        return injectionConfigBuilder;
    }

    public AbstractGenerator(String url, String username, String password, B customConfigBuilder) {
        this.dataSourceConfigBuilder = new DataSourceConfig.Builder(url, username, password)
                .typeConvertHandler((globalConfig, typeRegistry, metaInfo) -> {
                    // 避免byte转换成Integer
                    if (JdbcType.TINYINT == metaInfo.getJdbcType()) {
                        return DbColumnType.INTEGER;
                    }
                    return typeRegistry.getColumnType(metaInfo);
                })
        ;
        this.customConfigBuilder = customConfigBuilder;
        init();
    }

    public AbstractGenerator<C, B> mapperXmlResource(String mapperXmlResource) {
        String projectPath = System.getProperty("user.dir");
        packageConfigBuilder.pathInfo(Collections.singletonMap(OutputFile.mapper, projectPath + "/src/main/resources/" + mapperXmlResource));
        return this;
    }

    private void init() {
        String projectPath = System.getProperty("user.dir");
        String username = System.getProperty("user.name");
        if (username == null || username.isEmpty()) {
            username = "generator";
        }
        globalConfigBuilder
                .author(username)
                .dateType(DateType.TIME_PACK)
                .outputDir(projectPath + "/src/main/java")
        ;
        packageConfigBuilder
                .parent("io.github.bootystar")
                .xml("mapper")
        ;
        strategyConfigBuilder.entityBuilder()
//                .idType(IdType.ASSIGN_ID)
//                .logicDeleteColumnName("deleted")
        ;
        strategyConfigBuilder.controllerBuilder()
//                .enableRestStyle()
        ;
        strategyConfigBuilder.serviceBuilder()
//                .formatServiceFileName("%sService")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
        ;

        // 模板配置
//        templateConfigBuilder
//                .entity("/common/entity.java")
//                .controller("/common/controller.java")
//                .service("/common/service.java")
//                .serviceImpl("/common/serviceImpl.java")
//                .mapper("/common/mapper.java")
//                .xml("/common/mapper.xml")
//        ;

        strategyConfigBuilder.entityBuilder()
                .javaTemplate("/common/entity.java")
        ;
        strategyConfigBuilder.controllerBuilder()
                .template("/common/controller.java")
        ;
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/common/service.java")
                .serviceImplTemplate("/common/serviceImpl.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperTemplate("/common/mapper.java")
                .mapperXmlTemplate("/common/mapper.xml")
        ;

        // 子类配置
        config4child();
    }

    protected abstract void config4child();


    /**
     * 初始化一些常用配置项
     * <p>
     * 自定义:新增及修改排除createTime、updateTime属性, 排序默认使用create_time,id倒排
     * 实体类:雪花算法id,逻辑删除字段deleted;禁用SerialVersionUID,启用lombok
     * mapper: 无操作
     * service: 去掉IService后缀的I
     * controller: 启用restController
     *
     * @return {@link AbstractGenerator }<{@link C }, {@link B }>
     * @author bootystar
     */
    public AbstractGenerator<C, B> initialize() {
        customConfigBuilder()
                .insertExcludeFields(Arrays.asList("createTime", "updateTime"))
                .updateExcludeFields(Arrays.asList("createTime", "updateTime"))
                .orderColumn("create_time", true)
                .orderColumn("id", true)
        ;

        strategyConfigBuilder.entityBuilder()
                .idType(IdType.ASSIGN_ID)
                .logicDeleteColumnName("deleted")
                .disableSerialVersionUID()
                .enableLombok()
        ;
        strategyConfigBuilder.mapperBuilder()
//                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
        ;
        strategyConfigBuilder.serviceBuilder()
                .formatServiceFileName("%sService")
        ;
        strategyConfigBuilder.controllerBuilder()
                .enableRestStyle()
        ;
        return this;
    }

    public void execute(String... tableNames) {
        if (tableNames != null && tableNames.length > 0) {
            strategyConfigBuilder.addInclude(Arrays.asList(tableNames));
        }
        execute();
    }


    private void execute() {
        DataSourceConfig dataSourceConfig = dataSourceConfigBuilder.build();
        GlobalConfig globalConfig = globalConfigBuilder.build();

        StrategyConfig strategyConfig = strategyConfigBuilder.build();

//        TemplateConfig templateConfig = templateConfigBuilder.build();

        InjectionConfig injectionConfig = injectionConfigBuilder.build();

        PackageConfig packageConfig = packageConfigBuilder.build();


        ConfigBase customConfig = customConfigBuilder().build();

        String DTOPackage = customConfig.getDTOPackage().replaceAll("\\.", "\\" + File.separator);
        String VOPackage = customConfig.getVOPackage().replaceAll("\\.", "\\" + File.separator);
        LinkedList<CustomFile> customFiles = new LinkedList<>();

        Class<? extends IConfig> type = customConfig.getGeneratorType();

        if (customConfig.isGenerateInsert()) {
            CustomFile InsertDto = new CustomFile.Builder().fileName("InsertDTO.java").templatePath("/common/entityInsertDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(InsertDto);
        }
        if (customConfig.isGenerateUpdate()) {
            CustomFile updateDto = new CustomFile.Builder().fileName("UpdateDTO.java").templatePath("/common/entityUpdateDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(updateDto);
        }
        if (customConfig.isGenerateSelect()) {
            if (!type.equals(SplicingConfig.class)) {
                CustomFile selectDto = new CustomFile.Builder().fileName("SelectDTO.java").templatePath("/common/entitySelectDTO.java.vm").packageName(DTOPackage).build();
                customFiles.add(selectDto);
            }
        }
        if (customConfig.isGenerateExport() && !customConfig.isExportOnVO()) {
            CustomFile exportDto = new CustomFile.Builder().fileName("ExportDTO.java").templatePath("/common/entityExportDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(exportDto);
        }

        if (customConfig.isGenerateImport() && !customConfig.isImportOnVO()) {
            CustomFile importDto = new CustomFile.Builder().fileName("ImportDTO.java").templatePath("/common/entityImportDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(importDto);
        }

        CustomFile vo = new CustomFile.Builder().fileName("VO.java").templatePath("/common/entityVO.java.vm").packageName(VOPackage).build();
        customFiles.add(vo);


        customConfig.setCustomFiles(customFiles);

        BaseGenerator customGenerator =
                new BaseGenerator(dataSourceConfig)
                        .global(globalConfig)
                        // 包配置
                        .packageInfo(packageConfig)
                        // 策略配置
                        .strategy(strategyConfig)
                        // 注入配置
                        .injection(injectionConfig)
//                        // 模板配置
//                        .template(templateConfig)
                        // 自定义配置
                        .custom(customConfig);

        customGenerator.execute();
    }


}
