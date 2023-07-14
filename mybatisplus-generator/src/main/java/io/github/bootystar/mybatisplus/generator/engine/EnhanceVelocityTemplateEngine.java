package io.github.bootystar.mybatisplus.generator.engine;

import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.generator.config.ConstVal;
import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import com.baomidou.mybatisplus.generator.engine.VelocityTemplateEngine;
import io.github.bootystar.mybatisplus.generator.config.CustomConfig;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.List;
import java.util.Map;

/**
 * 自定义Velocity引擎
 * @Author booty
 * @Date 2023/7/13 13:40
 */
public class EnhanceVelocityTemplateEngine extends VelocityTemplateEngine {

    private CustomConfig customConfig;


    public EnhanceVelocityTemplateEngine() {
    }

    public EnhanceVelocityTemplateEngine(CustomConfig customConfig) {
        this.customConfig = customConfig;
    }

    @Override
    protected void outputCustomFile(@NotNull List<CustomFile> customFiles, @NotNull TableInfo tableInfo, @NotNull Map<String, Object> objectMap) {
        String entityName = tableInfo.getEntityName();
        String parentPath = getPathInfo(OutputFile.parent);
        customFiles.forEach(file -> {
            String filePath = StringUtils.isNotBlank(file.getFilePath()) ? file.getFilePath() : parentPath;
            if (StringUtils.isNotBlank(file.getPackageName())) {
                filePath = filePath + File.separator + file.getPackageName();
            }
            String fileName = filePath + File.separator + entityName + file.getFileName();
            outputFile(new File(fileName), objectMap, file.getTemplatePath(), file.isFileOverride());
        });
    }

    @Override
    public @NotNull Map<String, Object> getObjectMap(@NotNull ConfigBuilder config, @NotNull TableInfo tableInfo) {
        Map<String, Object> objectMap = super.getObjectMap(config, tableInfo);
        if (customConfig!=null){
            Map<String, Object> customData = customConfig.renderData(tableInfo);
            if (customData!=null){
                objectMap.putAll(customData);
            }
        }
       ;
        objectMap.put("basePackage",config.getPackageConfig().getParent());

        /*
        data.put("controllerMappingHyphen", StringUtils.camelToHyphen(tableInfo.getEntityPath()));
        data.put("controllerMappingHyphenStyle", this.hyphenStyle);
        data.put("restControllerStyle", this.restStyle);
        data.put("superControllerClassPackage", StringUtils.isBlank(superClass) ? null : superClass);
        data.put("superControllerClass", ClassUtils.getSimpleName(this.superClass));

        data.put("enableCache", enableCache);
        data.put("mapperAnnotation", mapperAnnotationClass != null);
        data.put("mapperAnnotationClass", mapperAnnotationClass);
        data.put("baseResultMap", this.baseResultMap);
        data.put("baseColumnList", this.baseColumnList);
        data.put("superMapperClassPackage", this.superClass);
        if (enableCache) {
            Class<? extends Cache> cacheClass = this.getCache();
            data.put("cache", cacheClass);
            data.put("cacheClassName", cacheClass.getName());
        }
        data.put("superMapperClass", ClassUtils.getSimpleName(this.superClass));


        data.put("superServiceClassPackage", this.superServiceClass);
        data.put("superServiceClass", ClassUtils.getSimpleName(this.superServiceClass));
        data.put("superServiceImplClassPackage", this.superServiceImplClass);
        data.put("superServiceImplClass", ClassUtils.getSimpleName(this.superServiceImplClass));


        data.put("idType", idType == null ? null : idType.toString());
        data.put("logicDeleteFieldName", this.logicDeleteColumnName);
        data.put("versionFieldName", this.versionColumnName);
        data.put("activeRecord", this.activeRecord);
        data.put("entitySerialVersionUID", this.serialVersionUID);
        data.put("entityColumnConstant", this.columnConstant);
        data.put("entityBuilderModel", this.chain);
        data.put("chainModel", this.chain);
        data.put("entityLombokModel", this.lombok);
        data.put("entityBooleanColumnRemoveIsPrefix", this.booleanColumnRemoveIsPrefix);
        data.put("superEntityClass", ClassUtils.getSimpleName(this.superClass));


        objectMap.put("config", config);
        objectMap.put("package", config.getPackageConfig().getPackageInfo());
        objectMap.put("author", globalConfig.getAuthor());
        objectMap.put("kotlin", globalConfig.isKotlin());
        objectMap.put("swagger", globalConfig.isSwagger());
        objectMap.put("springdoc", globalConfig.isSpringdoc());
        objectMap.put("date", globalConfig.getCommentDate());
        // 启用 schema 处理逻辑
        String schemaName = "";
        if (strategyConfig.isEnableSchema()) {
            // 存在 schemaName 设置拼接 . 组合表名
            schemaName = config.getDataSourceConfig().getSchemaName();
            if (StringUtils.isNotBlank(schemaName)) {
                schemaName += ".";
                tableInfo.setConvert(true);
            }
        }
        objectMap.put("schemaName", schemaName);
        objectMap.put("table", tableInfo);
        objectMap.put("entity", tableInfo.getEntityName());


         */

        return objectMap ;
    }


}
