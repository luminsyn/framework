/*
 * Copyright (c) 2011-2022, baomidou (jobob@qq.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.github.bootystar.mybatisplus.generator;

import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.generator.config.IConfig;
import io.github.bootystar.mybatisplus.generator.engine.EnhanceVelocityTemplateEngine;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;


/**
 * 自定义代码构造器
 * @author booty
 * @since 2023/07/13 14:18
 */
@Slf4j
public class CustomGenerator {

    /**
     * 配置信息
     */
    protected ConfigBuilder config;
    /**
     * 注入配置
     */
    protected InjectionConfig injection;
    /**
     * 数据源配置
     */
    private DataSourceConfig dataSource;
    /**
     * 数据库表配置
     */
    private StrategyConfig strategy;
    /**
     * 包 相关配置
     */
    private PackageConfig packageInfo;
    /**
     * 模板 相关配置
     */
    private TemplateConfig template;
    /**
     * 全局 相关配置
     */
    private GlobalConfig globalConfig;

    /**
     * 自定义相关配置
     */
    private IConfig custom;

    private CustomGenerator() {
        // 不推荐使用
    }

    /**
     * 构造方法
     *
     * @param dataSourceConfig 数据库配置
     * @since 3.5.0
     */
    public CustomGenerator(DataSourceConfig dataSourceConfig) {
        //这个是必须参数,其他都是可选的,后续去除默认构造更改成final
        this.dataSource = dataSourceConfig;
    }

    /**
     * 注入配置
     *
     * @param injectionConfig 注入配置
     * @return this
     * @since 3.5.0
     */
    public CustomGenerator injection(InjectionConfig injectionConfig) {
        this.injection = injectionConfig;
        return this;
    }

    /**
     * 生成策略
     *
     * @param strategyConfig 策略配置
     * @return this
     * @since 3.5.0
     */
    public CustomGenerator strategy(StrategyConfig strategyConfig) {
        this.strategy = strategyConfig;
        return this;
    }

    /**
     * 指定包配置信息
     *
     * @param packageConfig 包配置
     * @return this
     * @since 3.5.0
     */
    public CustomGenerator packageInfo(PackageConfig packageConfig) {
        this.packageInfo = packageConfig;
        return this;
    }

    /**
     * 指定模板配置
     *
     * @param templateConfig 模板配置
     * @return this
     * @since 3.5.0
     */
    public CustomGenerator template(TemplateConfig templateConfig) {
        this.template = templateConfig;
        return this;
    }

    /**
     * 指定全局配置
     *
     * @param globalConfig 全局配置
     * @return this
     * @see 3.5.0
     */
    public CustomGenerator global(GlobalConfig globalConfig) {
        this.globalConfig = globalConfig;
        return this;
    }

    /**
     * 设置配置汇总
     *
     * @param configBuilder 配置汇总
     * @return this
     * @since 3.5.0
     */
    public CustomGenerator config(ConfigBuilder configBuilder) {
        this.config = configBuilder;
        return this;
    }


    public CustomGenerator custom(IConfig config) {
        this.custom = config;
        return this;
    }

    private EnhanceVelocityTemplateEngine engine ;
    /**
     * 生成代码
     */
    public void execute() {

        log.debug("==========================准备生成文件...==========================");
        // 初始化配置
        if (null == config) {
            config = new ConfigBuilder(packageInfo, dataSource, strategy, template, globalConfig, injection);
        }
        EnhanceVelocityTemplateEngine templateEngine = new EnhanceVelocityTemplateEngine(custom);
        templateEngine.setConfigBuilder(config);
        // 模板引擎初始化执行文件输出
        templateEngine.init(config).batchOutput().open();
        log.debug("this generator is based on mybatis-plus 3.5.3 please use mybatis-plus 3.5.3 or above in your project \n" +
                "                                        )               )            )               )       \n" +
                " (  (     (           (   (       )  ( /(      (     ( /(  (      ( /(            ( /( (     \n" +
                " )\\))(   ))\\  (      ))\\  )(   ( /(  )\\()) (   )(    )\\()) )\\ )   )\\())  (    (   )\\()))\\ )  \n" +
                "((_))\\  /((_) )\\ )  /((_)(()\\  )(_))(_))/  )\\ (()\\  ((_)\\ (()/(  ((_)\\   )\\   )\\ (_))/(()/(  \n" +
                " (()(_)(_))  _(_/( (_))   ((_)((_)_ | |_  ((_) ((_) | |(_) )(_)) | |(_) ((_) ((_)| |_  )(_)) \n" +
                "/ _` | / -_)| ' \\))/ -_) | '_|/ _` ||  _|/ _ \\| '_| | '_ \\| || | | '_ \\/ _ \\/ _ \\|  _|| || | \n" +
                "\\__, | \\___||_||_| \\___| |_|  \\__,_| \\__|\\___/|_|   |_.__/ \\_, | |_.__/\\___/\\___/ \\__| \\_, | \n" +
                "|___/                                                      |__/                        |__/  ");
        log.debug("==========================文件生成完成！！！==========================\n");

    }





    /**
     * 开放表信息、预留子类重写
     *
     * @param config 配置信息
     * @return ignore
     */
    protected List<TableInfo> getAllTableInfoList(ConfigBuilder config) {
        return config.getTableInfoList();
    }

    public ConfigBuilder getConfig() {
        return config;
    }

    public InjectionConfig getInjectionConfig() {
        return injection;
    }

    public DataSourceConfig getDataSource() {
        return dataSource;
    }

    public StrategyConfig getStrategy() {
        return strategy;
    }

    public PackageConfig getPackageInfo() {
        return packageInfo;
    }

    public TemplateConfig getTemplate() {
        return template;
    }

    public GlobalConfig getGlobalConfig() {
        return globalConfig;
    }

    public IConfig getCustom() {
        return custom;
    }
}
