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
public abstract class AbstractGenerator {

    protected DataSourceConfig.Builder dataSourceConfigBuilder;

    protected GlobalConfig.Builder globalConfigBuilder = new GlobalConfig.Builder();

    protected PackageConfig.Builder packageConfigBuilder = new PackageConfig.Builder();

    protected StrategyConfig.Builder strategyConfigBuilder = new StrategyConfig.Builder();

    protected InjectionConfig.Builder injectionConfigBuilder = new InjectionConfig.Builder();

    protected TemplateConfig.Builder templateConfigBuilder = new TemplateConfig.Builder();

    public abstract ConfigBaseBuilder<?, ?> customConfigBuilder();

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

    /**
     * 模板配置生成器
     *
     * @return {@link TemplateConfig.Builder }
     * @author bootystar
     * @deprecated 自MP3.5.6开始废弃使用, 3.5.6之后版本请使用{@link #strategyConfigBuilder()}
     */
    @Deprecated
    public TemplateConfig.Builder templateConfigBuilder() {
        return templateConfigBuilder;
    }

    public InjectionConfig.Builder injectionConfigBuilder() {
        return injectionConfigBuilder;
    }

    public AbstractGenerator(String url, String username, String password) {
        this.dataSourceConfigBuilder = new DataSourceConfig.Builder(url, username, password)
                .typeConvertHandler((globalConfig, typeRegistry, metaInfo) -> {
                    // 兼容旧版本转换成Integer
                    if (JdbcType.TINYINT == metaInfo.getJdbcType()) {
                        return DbColumnType.INTEGER;
                    }
                    return typeRegistry.getColumnType(metaInfo);
                })
        ;
    }

    public AbstractGenerator mapperPackage(String mapperPackage) {
        String projectPath = System.getProperty("user.dir");
        packageConfigBuilder.pathInfo(Collections.singletonMap(OutputFile.mapper, projectPath + "/src/main/resources/" + mapperPackage));
        return this;
    }

    protected void init() {
        String projectPath = System.getProperty("user.dir");
        globalConfigBuilder
                .author("bootystar")
                .dateType(DateType.TIME_PACK)
                .outputDir(projectPath + "/src/main/java")
        ;
        // 兼容旧版
        templateConfigBuilder
                .entity("/common/entity.java")
                .controller("/common/controller.java")
                .service("/common/service.java")
                .serviceImpl("/common/serviceImpl.java")
                .mapper("/common/mapper.java")
                .xml("/common/mapper.xml")
        ;
        packageConfigBuilder.parent("io.github.bootystar").xml("mapper")
        ;
        strategyConfigBuilder.entityBuilder()
                .idType(IdType.ASSIGN_ID)
                .javaTemplate("/common/entity.java")
        ;
        strategyConfigBuilder.controllerBuilder()
                .template("/common/controller.java")
                .enableRestStyle()
        ;
        strategyConfigBuilder.serviceBuilder()
                .formatServiceFileName("%sService")
                .serviceTemplate("/common/service.java")
                .serviceImplTemplate("/common/serviceImpl.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
                .mapperXmlTemplate("/common/mapper.xml")
                .mapperTemplate("/common/mapper.java")
        ;
        customConfigBuilder().insertExcludeFields(Arrays.asList("createTime", "updateTime"));
        customConfigBuilder().updateExcludeFields(Arrays.asList("createTime", "updateTime"));
        customConfigBuilder().orderColumn("create_time", true);
        customConfigBuilder().orderColumn("id", true);
    }


    public void execute(String... tableNames) {
        strategyConfigBuilder.addInclude(Arrays.asList(tableNames));
        execute();
    }


    public void execute() {
        DataSourceConfig dataSourceConfig = dataSourceConfigBuilder.build();
        GlobalConfig globalConfig = globalConfigBuilder.build();

        StrategyConfig strategyConfig = strategyConfigBuilder.build();


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
            if (!type.equals(SplicingConfig.class)){
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
                        // 自定义配置
                        .custom(customConfig);

        customGenerator.execute();
    }


}
