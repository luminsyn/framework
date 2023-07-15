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
package io.github.bootystar.mybatisplus.generator.config;

import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import com.baomidou.mybatisplus.generator.config.TemplateType;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import com.baomidou.mybatisplus.generator.util.ClassUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * 自定义配置
 * 模板路径配置项
 *
 * @author booty
 * @date 2023/07/13 13:59
 * @since 2017-06-17
 */
public class CustomConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(CustomConfig.class);


    /**
     * 返回结果类
     */
    private String returnResultClass;
    /**
     * 返回结果类所在包
     */
    private String returnResultClassPackage;
    /**
     * 返回结果是否支持泛型
     */
    private Boolean returnResultGenericType ;
    /**
     * 返回结果静态方法名
     */
    private String returnResultDefaultStaticMethodName;
    /**
     * 生成分页查询方法
     */
    private Boolean pageByDto;
    /**
     * 生成excel导出方法
     */
    private Boolean exportExcel;
    /**
     * 新增排除的字段
     */
    private List<String> insertExcludeFields = new LinkedList<>();
    /**
     * 修改排除的字段
     */
    private List<String> updateExcludeFields = new LinkedList<>();
    /**
     * 查询排除的字段
     */
    private List<String> selectExcludeFields = new LinkedList<>();


    private String shift3="#";
    private String shift4="$";
    private String shift5="%";
    private String shift8="*";
    private String shiftLeft="{";
    private String shiftRight="}";



    /**
     * 不对外爆露
     */
    private CustomConfig() {

    }


    /**
     * 呈现数据
     *
     * @param tableInfo 表信息
     * @return {@code Map<String, Object> }
     * @author booty
     * @date 2023/07/13 14:01
     */
    public Map<String, Object> renderData(TableInfo tableInfo) {
        HashMap<String, Object> data = new HashMap<>();
        try {
            for (Field field : this.getClass().getDeclaredFields()) {
                String name = field.getName();
                field.setAccessible(true);
                data.put(name,field.get(this));
            }
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
//        data.put("returnResultClass", ClassUtils.getSimpleName(this.returnResultClass));
//        data.put("returnResultClassPackage", this.returnResultClass);
        data.put("nowTime", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

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
        return data;
    }


    /**
     * 模板路径配置构建者
     *
     * @author booty 3.5.3
     */
    public static class Builder implements IConfigBuilder<CustomConfig> {
        private final CustomConfig customConfig;

        /**
         * 默认生成一个空的
         */
        public Builder() {
            this.customConfig = new CustomConfig();
        }

        /**
         * 返回结果类
         *
         * @param returnResultClass 返回结果类
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/13 16:12
         */
        public Builder returnResultClass(@NotNull Class<?> returnResultClass){
            String fullClassName = returnResultClass.getName();
            this.customConfig.returnResultClassPackage= fullClassName;
            this.customConfig.returnResultClass=ClassUtils.getSimpleName(fullClassName);
            return this;
        }

        /**
         * 返回结果类
         *
         * @param fullClassName 返回结果类
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/13 16:12
         */
        public Builder returnResultClass(@NotNull String fullClassName){
            this.customConfig.returnResultClassPackage= fullClassName;
            this.customConfig.returnResultClass=ClassUtils.getSimpleName(fullClassName);
            return this;
        }


        /**
         * 返回结果是否为泛型类型
         *
         * @param isGenericType 是泛型类型
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 09:20
         */
        public Builder returnResultGenericType(@NotNull Boolean isGenericType){
            this.customConfig.returnResultGenericType=isGenericType;
            return this;
        }

        /**
         * 返回结果静态方法名字
         *
         * @param name 名字
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 09:20
         */
        public Builder returnResultDefaultStaticMethodName(@NotNull String name){
            this.customConfig.returnResultDefaultStaticMethodName=name;
            return this;
        }

        /**
         * 生成分页查询方法
         *
         * @param generate 生成
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 10:04
         */
        public Builder pageByDto(@NotNull Boolean generate){
            this.customConfig.pageByDto=generate;
            return this;
        }

        /**
         * 生成excel导出方法
         *
         * @param generate 生成
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 10:03
         */
        public Builder exportExcel(@NotNull Boolean generate){
            this.customConfig.exportExcel=generate;
            return this;
        }


        /**
         * 添加插入排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 15:38
         */
        public Builder insertExcludeFields(@NotNull List<String> fieldNames){
            this.customConfig.insertExcludeFields.addAll(fieldNames);
            return this;
        }

        /**
         * 添加更新排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 15:38
         */
        public Builder updateExcludeFields(@NotNull List<String> fieldNames){
            this.customConfig.updateExcludeFields.addAll(fieldNames);
            return this;
        }

        /**
         * 添加查询排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 15:38
         */
        public Builder selectExcludeFields(@NotNull List<String> fieldNames){
            this.customConfig.selectExcludeFields.addAll(fieldNames);
            return this;
        }

        /**
         * 添加插入排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 15:37
         */
        public Builder insertExcludeField(@NotNull String...  fieldNames){
            this.customConfig.insertExcludeFields.addAll(Arrays.asList(fieldNames));
            return this;
        }

        /**
         * 添加更新排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 15:37
         */
        public Builder updateExcludeField(@NotNull String...  fieldNames){
            this.customConfig.updateExcludeFields.addAll(Arrays.asList(fieldNames));
            return this;
        }

        /**
         * 添加查询排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/14 15:37
         */
        public Builder selectExcludeField(@NotNull String...  fieldNames){
            this.customConfig.selectExcludeFields.addAll(Arrays.asList(fieldNames));
            return this;
        }




        /**
         * 构建模板配置对象
         *
         * @return 模板配置对象
         */
        @Override
        public CustomConfig build() {
            return this.customConfig;
        }
    }
}
