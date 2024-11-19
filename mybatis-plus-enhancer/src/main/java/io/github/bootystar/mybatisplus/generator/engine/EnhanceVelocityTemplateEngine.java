package io.github.bootystar.mybatisplus.generator.engine;

import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.TemplateConfig;
import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import com.baomidou.mybatisplus.generator.engine.AbstractTemplateEngine;
import com.baomidou.mybatisplus.generator.engine.VelocityTemplateEngine;
import io.github.bootystar.mybatisplus.config.base.IConfig;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 自定义Velocity引擎
 *
 * @author bootystar
 */
public class EnhanceVelocityTemplateEngine extends VelocityTemplateEngine {

    private final IConfig customConfig;

    public EnhanceVelocityTemplateEngine(IConfig customConfig) {
        this.customConfig = customConfig;
    }

    @Override
    protected void outputCustomFile(List<CustomFile> customFiles, TableInfo tableInfo, Map<String, Object> objectMap) {
        String entityName = tableInfo.getEntityName();
        String parentPath = getPathInfo(OutputFile.parent);
        boolean fileOverride = customConfig.getFileOverride();
        customFiles.forEach(file -> {
            String filePath = StringUtils.isNotBlank(file.getFilePath()) ? file.getFilePath() : parentPath;
            if (StringUtils.isNotBlank(file.getPackageName())) {
                filePath = filePath + File.separator + file.getPackageName();
            }
            String fileName = filePath + File.separator + entityName + file.getFileName();

//            outputFile(new File(fileName), objectMap, file.getTemplatePath(), file.isFileOverride());
            outputFile(new File(fileName), objectMap, file.getTemplatePath(), fileOverride);
        });
    }

    @Override
    public Map<String, Object> getObjectMap(ConfigBuilder config, TableInfo tableInfo) {
        Map<String, Object> objectMap = super.getObjectMap(config, tableInfo);
        if (customConfig != null) {
            Map<String, Object> customData = customConfig.renderData(tableInfo);
            if (customData != null) {
                objectMap.putAll(customData);
            }
        }
        objectMap.put("basePackage", config.getPackageConfig().getParent());

        return objectMap;
    }


    @Override
    public AbstractTemplateEngine batchOutput() {
        try {
            ConfigBuilder config = this.getConfigBuilder();
            List<TableInfo> tableInfoList = config.getTableInfoList();
            tableInfoList.forEach(tableInfo -> {
                Map<String, Object> objectMap = this.getObjectMap(config, tableInfo);
                Optional.ofNullable(config.getInjectionConfig()).ifPresent(t -> {
                    // 添加自定义属性
                    t.beforeOutputFile(tableInfo, objectMap);
                    // 输出自定义文件
                    outputCustomFile(t.getCustomFiles(), tableInfo, objectMap);
                });
                // entity
                outputEntity(tableInfo, objectMap);
                // mapper and xml
                outputMapper(tableInfo, objectMap);
                // service
                outputService(tableInfo, objectMap);
                // controller
                outputController(tableInfo, objectMap);

                Optional.ofNullable(customConfig).ifPresent(t -> {
                    // 输出自定义文件
                    outputCustomFile(t.getCustomFiles(), tableInfo, objectMap);
                });

            });
        } catch (Exception e) {
            throw new RuntimeException("无法创建文件，请检查配置信息！", e);
        }
        return this;
    }


//    protected void outputEntity(TableInfo tableInfo, Map<String, Object> objectMap) {
//        String entityName = tableInfo.getEntityName();
//        String entityPath = this.getPathInfo(OutputFile.entity);
//        if (StringUtils.isNotBlank(entityName) && StringUtils.isNotBlank(entityPath)) {
//            this.getTemplateFilePath((template) -> {
//                return template.getEntity(this.getConfigBuilder().getGlobalConfig().isKotlin());
//            }).ifPresent((entity) -> {
//                String entityFile = String.format(entityPath + File.separator + "%s" + this.suffixJavaOrKt(), entityName);
//                this.outputFile(new File(entityFile), objectMap, entity, this.getConfigBuilder().getStrategyConfig().entity().isFileOverride());
//            });
//        }
//
//    }
//
//    protected void outputMapper(TableInfo tableInfo, Map<String, Object> objectMap) {
//        String entityName = tableInfo.getEntityName();
//        String mapperPath = this.getPathInfo(OutputFile.mapper);
//        if (StringUtils.isNotBlank(tableInfo.getMapperName()) && StringUtils.isNotBlank(mapperPath)) {
//            this.getTemplateFilePath(TemplateConfig::getMapper).ifPresent((mapper) -> {
//                String mapperFile = String.format(mapperPath + File.separator + tableInfo.getMapperName() + this.suffixJavaOrKt(), entityName);
//                this.outputFile(new File(mapperFile), objectMap, mapper, this.getConfigBuilder().getStrategyConfig().mapper().isFileOverride());
//            });
//        }
//
//        String xmlPath = this.getPathInfo(OutputFile.xml);
//        if (StringUtils.isNotBlank(tableInfo.getXmlName()) && StringUtils.isNotBlank(xmlPath)) {
//            this.getTemplateFilePath(TemplateConfig::getXml).ifPresent((xml) -> {
//                String xmlFile = String.format(xmlPath + File.separator + tableInfo.getXmlName() + ".xml", entityName);
//                this.outputFile(new File(xmlFile), objectMap, xml, this.getConfigBuilder().getStrategyConfig().mapper().isFileOverride());
//            });
//        }
//
//    }
//
//    protected void outputService(TableInfo tableInfo, Map<String, Object> objectMap) {
//        String entityName = tableInfo.getEntityName();
//        String servicePath = this.getPathInfo(OutputFile.service);
//        if (StringUtils.isNotBlank(tableInfo.getServiceName()) && StringUtils.isNotBlank(servicePath)) {
//            this.getTemplateFilePath(TemplateConfig::getService).ifPresent((service) -> {
//                String serviceFile = String.format(servicePath + File.separator + tableInfo.getServiceName() + this.suffixJavaOrKt(), entityName);
//                this.outputFile(new File(serviceFile), objectMap, service, this.getConfigBuilder().getStrategyConfig().service().isFileOverride());
//            });
//        }
//
//        String serviceImplPath = this.getPathInfo(OutputFile.serviceImpl);
//        if (StringUtils.isNotBlank(tableInfo.getServiceImplName()) && StringUtils.isNotBlank(serviceImplPath)) {
//            this.getTemplateFilePath(TemplateConfig::getServiceImpl).ifPresent((serviceImpl) -> {
//                String implFile = String.format(serviceImplPath + File.separator + tableInfo.getServiceImplName() + this.suffixJavaOrKt(), entityName);
//                this.outputFile(new File(implFile), objectMap, serviceImpl, this.getConfigBuilder().getStrategyConfig().service().isFileOverride());
//            });
//        }
//
//    }
//
//    protected void outputController(TableInfo tableInfo, Map<String, Object> objectMap) {
//        String controllerPath = this.getPathInfo(OutputFile.controller);
//        if (StringUtils.isNotBlank(tableInfo.getControllerName()) && StringUtils.isNotBlank(controllerPath)) {
//            this.getTemplateFilePath(TemplateConfig::getController).ifPresent((controller) -> {
//                String entityName = tableInfo.getEntityName();
//                String controllerFile = String.format(controllerPath + File.separator + tableInfo.getControllerName() + this.suffixJavaOrKt(), entityName);
//                this.outputFile(new File(controllerFile), objectMap, controller, this.getConfigBuilder().getStrategyConfig().controller().isFileOverride());
//            });
//        }
//    }


//   3.5.6 OR Above
//    protected void outputController(@NotNull TableInfo tableInfo, @NotNull Map<String, Object> objectMap) {
//        // MpController.java
//        Controller controller = this.getConfigBuilder().getStrategyConfig().controller();
//        String controllerPath = getPathInfo(OutputFile.controller);
//        if (controller.isGenerate()) {
//            String entityName = tableInfo.getEntityName();
//            String controllerFile = String.format((controllerPath + File.separator + tableInfo.getControllerName() + suffixJavaOrKt()), entityName);
//            outputFile(getOutputFile(controllerFile, OutputFile.controller), objectMap, templateFilePath(controller.getTemplatePath()), getConfigBuilder().getStrategyConfig().controller().isFileOverride());
//        }
//    }
//
//    protected void outputService(@NotNull TableInfo tableInfo, @NotNull Map<String, Object> objectMap) {
//        // IMpService.java
//        String entityName = tableInfo.getEntityName();
//        // 判断是否要生成service接口
//        Service service = this.getConfigBuilder().getStrategyConfig().service();
//        if (service.isGenerateService()) {
//            String servicePath = getPathInfo(OutputFile.service);
//            String serviceFile = String.format((servicePath + File.separator + tableInfo.getServiceName() + suffixJavaOrKt()), entityName);
//            outputFile(getOutputFile(serviceFile, OutputFile.service), objectMap, templateFilePath(service.getServiceTemplate()), getConfigBuilder().getStrategyConfig().service().isFileOverride());
//        }
//        // MpServiceImpl.java
//        String serviceImplPath = getPathInfo(OutputFile.serviceImpl);
//        if (service.isGenerateServiceImpl()) {
//            String implFile = String.format((serviceImplPath + File.separator + tableInfo.getServiceImplName() + suffixJavaOrKt()), entityName);
//            outputFile(getOutputFile(implFile, OutputFile.serviceImpl), objectMap, templateFilePath(service.getServiceImplTemplate()), getConfigBuilder().getStrategyConfig().service().isFileOverride());
//        }
//    }
//
//    protected void outputMapper(@NotNull TableInfo tableInfo, @NotNull Map<String, Object> objectMap) {
//        // MpMapper.java
//        String entityName = tableInfo.getEntityName();
//        String mapperPath = getPathInfo(OutputFile.mapper);
//        Mapper mapper = this.getConfigBuilder().getStrategyConfig().mapper();
//        if (mapper.isGenerateMapper()) {
//            String mapperFile = String.format((mapperPath + File.separator + tableInfo.getMapperName() + suffixJavaOrKt()), entityName);
//            outputFile(getOutputFile(mapperFile, OutputFile.mapper), objectMap, templateFilePath(mapper.getMapperTemplatePath()), getConfigBuilder().getStrategyConfig().mapper().isFileOverride());
//        }
//        // MpMapper.xml
//        String xmlPath = getPathInfo(OutputFile.xml);
//        if (mapper.isGenerateMapperXml()) {
//            String xmlFile = String.format((xmlPath + File.separator + tableInfo.getXmlName() + ConstVal.XML_SUFFIX), entityName);
//            outputFile(getOutputFile(xmlFile, OutputFile.xml), objectMap, templateFilePath(mapper.getMapperXmlTemplatePath()), getConfigBuilder().getStrategyConfig().mapper().isFileOverride());
//        }
//    }
//
//    protected void outputEntity(@NotNull TableInfo tableInfo, @NotNull Map<String, Object> objectMap) {
//        String entityName = tableInfo.getEntityName();
//        String entityPath = getPathInfo(OutputFile.entity);
//        Entity entity = this.getConfigBuilder().getStrategyConfig().entity();
//        GlobalConfig globalConfig = this.getConfigBuilder().getGlobalConfig();
//        if (entity.isGenerate()) {
//            String entityFile = String.format((entityPath + File.separator + "%s" + suffixJavaOrKt()), entityName);
//            outputFile(getOutputFile(entityFile, OutputFile.entity), objectMap, templateFilePath(globalConfig.isKotlin() ? entity.getKotlinTemplate() : entity.getJavaTemplate()), getConfigBuilder().getStrategyConfig().entity().isFileOverride());
//        }
//    }


}
