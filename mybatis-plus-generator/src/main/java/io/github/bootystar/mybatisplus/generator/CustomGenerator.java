package io.github.bootystar.mybatisplus.generator;

import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import io.github.bootystar.mybatisplus.generator.config.IConfig;
import io.github.bootystar.mybatisplus.generator.engine.EnhanceVelocityTemplateEngine;
import lombok.Getter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;


/**
 * 自定义配置代码生成器
 * @author booty
 */
@Getter
public class CustomGenerator {
    private static final Logger log = LoggerFactory.getLogger(CustomGenerator.class);

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

        log.debug("==========================文件生成完成！！！==========================");
//        System.out.println(
//                "                                        )               )            )               )       \n" +
//                        " (  (     (           (   (       )  ( /(      (     ( /(  (      ( /(            ( /( (     \n" +
//                        " )\\))(   ))\\  (      ))\\  )(   ( /(  )\\()) (   )(    )\\()) )\\ )   )\\())  (    (   )\\()))\\ )  \n" +
//                        "((_))\\  /((_) )\\ )  /((_)(()\\  )(_))(_))/  )\\ (()\\  ((_)\\ (()/(  ((_)\\   )\\   )\\ (_))/(()/(  \n" +
//                        " (()(_)(_))  _(_/( (_))   ((_)((_)_ | |_  ((_) ((_) | |(_) )(_)) | |(_) ((_) ((_)| |_  )(_)) \n" +
//                        "/ _` | / -_)| ' \\))/ -_) | '_|/ _` ||  _|/ _ \\| '_| | '_ \\| || | | '_ \\/ _ \\/ _ \\|  _|| || | \n" +
//                        "\\__, | \\___||_||_| \\___| |_|  \\__,_| \\__|\\___/|_|   |_.__/ \\_, | |_.__/\\___/\\___/ \\__| \\_, | \n" +
//                        "|___/                                                      |__/                        |__/  \n");
        System.out.println(
                "___.                  __                   __                \n" +
                "\\_ |__   ____   _____/  |_ ___.__. _______/  |______ _______ \n" +
                " | __ \\ /  _ \\ /  _ \\   __<   |  |/  ___/\\   __\\__  \\\\_  __ \\\n" +
                " | \\_\\ (  <_> |  <_> )  |  \\___  |\\___ \\  |  |  / __ \\|  | \\/\n" +
                " |___  /\\____/ \\____/|__|  / ____/____  > |__| (____  /__|   \n" +
                "     \\/                    \\/         \\/            \\/       "
                );

        System.out.println("execute success! check files in following folder:");
        String path = config.getPathInfo().get(OutputFile.parent);
        System.out.println(new File(path).getAbsolutePath());
    }

}
