package io.github.bootystar.mybatisplus.generator.base;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.*;
import com.baomidou.mybatisplus.generator.config.rules.DateType;
import com.baomidou.mybatisplus.generator.config.rules.DbColumnType;
import io.github.bootystar.mybatisplus.generator.config.base.CustomConfig;
import io.github.bootystar.mybatisplus.generator.config.base.CustomConfigBase;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.JdbcType;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.function.Consumer;

/**
 * 自定义配置代码生成器-抽象父类
 *
 * @author bootystar
 */
@Slf4j
@SuppressWarnings("unused")
public abstract class GeneratorBase<B extends CustomConfigBase.Builder<?, B>> {

    protected DataSourceConfig.Builder dataSourceConfigBuilder;
    @Getter
    protected GlobalConfig.Builder globalConfigBuilder = new GlobalConfig.Builder();
    @Getter
    protected PackageConfig.Builder packageConfigBuilder = new PackageConfig.Builder();
    @Getter
    protected StrategyConfig.Builder strategyConfigBuilder = new StrategyConfig.Builder();
    @Getter
    protected InjectionConfig.Builder injectionConfigBuilder = new InjectionConfig.Builder();
    @Getter
    protected CustomConfigBase.Builder<?, B> customConfigBuilder;

    public GeneratorBase(String url, String username, String password, B customConfigBuilder) {
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
                .javaTemplate("/common/entity.java")
        ;
        strategyConfigBuilder.mapperBuilder()
                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
                .mapperTemplate("/common/mapper.java")
                .mapperXmlTemplate("/common/mapper.xml")
        ;
        strategyConfigBuilder.serviceBuilder()
                .serviceTemplate("/common/service.java")
                .serviceImplTemplate("/common/serviceImpl.java")
        ;
        strategyConfigBuilder.controllerBuilder()
                .template("/common/controller.java")
        ;

        // 子类配置
        config4child();
    }

    protected abstract void config4child();

    private void execute() {
        DataSourceConfig dataSourceConfig = dataSourceConfigBuilder.build();
        GlobalConfig globalConfig = globalConfigBuilder.build();
        PackageConfig packageConfig = packageConfigBuilder.build();
        StrategyConfig strategyConfig = strategyConfigBuilder.build();
        InjectionConfig injectionConfig = injectionConfigBuilder.build();
        CustomConfigBase customConfig = customConfigBuilder.build();
        String DTOPackage = customConfig.getPackage4DTO().replaceAll("\\.", "\\" + File.separator);
        String VOPackage = customConfig.getPackage4VO().replaceAll("\\.", "\\" + File.separator);
        ArrayList<CustomFile> customFiles = new ArrayList<>(8);
        if (customConfig.isGenerateInsert()) {
            CustomFile InsertDto = new CustomFile.Builder().fileName("InsertDTO.java").templatePath("/common/entityInsertDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(InsertDto);
        }
        if (customConfig.isGenerateUpdate()) {
            CustomFile updateDto = new CustomFile.Builder().fileName("UpdateDTO.java").templatePath("/common/entityUpdateDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(updateDto);
        }
        if (customConfig.isGenerateSelect() && customConfig.isGenerateSelectDTO()) {
            CustomFile selectDto = new CustomFile.Builder().fileName("SelectDTO.java").templatePath("/common/entitySelectDTO.java.vm").packageName(DTOPackage).build();
            customFiles.add(selectDto);
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
        CustomGenerator customGenerator = new CustomGenerator(dataSourceConfig)
                .global(globalConfig)
                .packageInfo(packageConfig)
                .strategy(strategyConfig)
                .injection(injectionConfig)
                .custom(customConfig)
                ;
        customGenerator.execute();
    }

    /**
     * 执行
     *
     * @param tableNames 表名(不填为全部)
     * @author bootystar
     */
    public void execute(String... tableNames) {
        if (tableNames != null && tableNames.length > 0) {
            strategyConfigBuilder.addInclude(Arrays.asList(tableNames));
        }
        execute();
    }

    public GeneratorBase<B> global(Consumer<GlobalConfig.Builder> consumer) {
        consumer.accept(globalConfigBuilder);
        return this;
    }

    public GeneratorBase<B> pkg(Consumer<PackageConfig.Builder> consumer) {
        consumer.accept(packageConfigBuilder);
        return this;
    }

    public GeneratorBase<B> strategy(Consumer<StrategyConfig.Builder> consumer) {
        consumer.accept(strategyConfigBuilder);
        return this;
    }

    public GeneratorBase<B> entity(Consumer<Entity.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.entityBuilder());
        return this;
    }

    public GeneratorBase<B> mapper(Consumer<Mapper.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.mapperBuilder());
        return this;
    }

    public GeneratorBase<B> service(Consumer<Service.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.serviceBuilder());
        return this;
    }

    public GeneratorBase<B> controller(Consumer<Controller.Builder> consumer) {
        consumer.accept(strategyConfigBuilder.controllerBuilder());
        return this;
    }

    public GeneratorBase<B> injection(Consumer<InjectionConfig.Builder> consumer) {
        consumer.accept(injectionConfigBuilder);
        return this;
    }

    public GeneratorBase<B> custom(Consumer<CustomConfigBase.Builder<?, B>> consumer) {
        consumer.accept(customConfigBuilder);
        return this;
    }

    /**
     * mapper.xml文件在项目根目录/src/main/resources/下的对应目录
     *
     * @param path 目录
     * @return {@link GeneratorBase }<{@link B }>
     * @author bootystar
     */
    public GeneratorBase<B> mapperXmlResource(String path) {
        String projectPath = System.getProperty("user.dir");
        if (path.startsWith("/")) {
            path = path.substring(1);
        }
        packageConfigBuilder.pathInfo(Collections.singletonMap(OutputFile.mapper, projectPath + "/src/main/resources/" + path));
        return this;
    }

    /**
     * 初始化推荐配置项
     *
     * @return {@link GeneratorBase }<{@link B }>
     * @author bootystar
     */
    public GeneratorBase<B> initialize() {
        customConfigBuilder
                .insertExcludeColumns(Arrays.asList("create_time", "update_time"))
                .updateExcludeColumns(Arrays.asList("create_time", "update_time"))
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
                .mapperAnnotation(org.apache.ibatis.annotations.Mapper.class)
        ;
        strategyConfigBuilder.serviceBuilder()
                .formatServiceFileName("%sService")
        ;
        strategyConfigBuilder.controllerBuilder()
                .enableRestStyle()
        ;
        return this;
    }


    /**
     * 启用全局文件覆盖
     *
     * @return {@link GeneratorBase }<{@link B }>
     * @author bootystar
     */
    public GeneratorBase<B> enableGlobalFileOverwrite() {
        customConfigBuilder
                .enableFileOverride()
        ;
        strategyConfigBuilder.entityBuilder()
                .enableFileOverride()
        ;
        strategyConfigBuilder.mapperBuilder()
                .enableFileOverride()
        ;
        strategyConfigBuilder.serviceBuilder()
                .enableFileOverride()
        ;
        strategyConfigBuilder.controllerBuilder()
                .enableFileOverride()
        ;
        return this;
    }

}
