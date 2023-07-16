package io.github.bootystar.mybatisplus.generator;

import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import io.github.bootystar.mybatisplus.generator.config.CustomConfig;
import io.github.bootystar.mybatisplus.generator.core.ReturnResult;

import java.util.Arrays;
import java.util.HashMap;

/**
 * @Author booty
 * @Date 2023/7/13 15:34
 * @see  com.baomidou.mybatisplus.generator.config.ConstVal;
 */

public class CrudGenerator extends BaseGenerator {
    public CrudGenerator(String url, String username, String password) {
        super(url, username, password);
    }

    @Override
    protected void init() {
        super.init();

        strategyConfigBuilder.controllerBuilder().enableRestStyle();
        templateConfigBuilder.controller("/crud/controller.java");
        templateConfigBuilder.service("/crud/service.java");
        templateConfigBuilder.serviceImpl("/crud/serviceImpl.java");
        templateConfigBuilder.mapper("/crud/mapper.java");
        templateConfigBuilder.xml("/crud/mapper.xml");
        templateConfigBuilder.entity("/crud/entity.java");
        customConfigBuilder.returnResultClass(ReturnResult.class);
        customConfigBuilder.returnResultGenericType(true);
        customConfigBuilder.returnResultDefaultStaticMethodName("success");
        customConfigBuilder.pageByDto(true);
        customConfigBuilder.exportExcel(true);
        customConfigBuilder.insertExcludeField("createTime","updateTime","version");
        customConfigBuilder.updateExcludeField("createTime","updateTime");
        customConfigBuilder.selectExcludeField("version");
    }

    @Override
    public void execute() {

        DataSourceConfig dataSourceConfig = dataSourceConfigBuilder.build();

        GlobalConfig globalConfig = globalConfigBuilder.build();

        StrategyConfig strategyConfig = strategyConfigBuilder.build();

        TemplateConfig templateConfig = templateConfigBuilder.build();



        CustomConfig customConfig = customConfigBuilder.build();
        String dtoPackage = customConfig.getDtoPackage();
        String voPackage = customConfig.getVoPackage();

        CustomFile InsertDto = new CustomFile.Builder().fileName("InsertDto.java").templatePath("/crud/entityInsertDto.java.vm").packageName(dtoPackage).build();
        CustomFile updateDto = new CustomFile.Builder().fileName("UpdateDto.java").templatePath("/crud/entityUpdateDto.java.vm").packageName(dtoPackage).build();
        CustomFile selectDto = new CustomFile.Builder().fileName("SelectDto.java").templatePath("/crud/entitySelectDto.java.vm").packageName(dtoPackage).build();
        CustomFile vo = new CustomFile.Builder().fileName("Vo.java").templatePath("/crud/entityVo.java.vm").packageName(voPackage).build();
        injectionConfigBuilder.customFile(Arrays.asList(InsertDto,updateDto,selectDto,vo));
        InjectionConfig injectionConfig = injectionConfigBuilder.build();


        CustomGenerator customGenerator =
                new CustomGenerator(dataSourceConfig)
                        .global(globalConfig)
                        // 包配置
                        .packageInfo(packageConfigBuilder.build())
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
