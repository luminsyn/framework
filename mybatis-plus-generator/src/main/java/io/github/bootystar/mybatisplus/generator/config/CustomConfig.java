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


import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import com.baomidou.mybatisplus.generator.util.ClassUtils;
import org.apache.ibatis.type.JdbcType;
import org.jetbrains.annotations.NotNull;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * 自定义配置
 * 模板路径配置项
 *
 * @author booty
 * @date 2023/07/13 13:59
 * @since 2017-06-17
 */
public class CustomConfig {

//    private static final Logger LOGGER = LoggerFactory.getLogger(CustomConfig.class);

    //--------------返回结果相关配置---------------

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
    private Boolean returnResultGenericType;

    /**
     * 返回结果静态方法名
     */
    private String returnResultDefaultStaticMethodName;

    /**
     * controller是否使用@RequestBody注解
     */
    private Boolean requestBody;

    /**
     * 是否添加参数校验
     */
    private Boolean enableValidated;
    /**
     * 是否添加跨域注解
     */
    private Boolean enableOrigins;


    /**
     * 新增排除的字段
     */
    private final Set<String> insertExcludeFields = new HashSet<>();

    /**
     * 修改排除的字段
     */
    private final Set<String> updateExcludeFields = new HashSet<>();

    /**
     * 生成分页扩展查询方法
     */
    private Boolean pageByDto;

    /**
     * 生成excel导出方法（基于分页扩展查询方法）
     */
    private Boolean exportExcel;


    /**
     * vo所在包
     */
    private String voPackage = "vo";
    /**
     * dto所在包
     */
    private String dtoPackage = "dto";


    /**
     * 自定义模板文件列表
     */
    private final List<CustomFile> customFiles = new ArrayList<>();

    /**
     * vo是否继承entity
     */
    private Boolean voExtendsEntity;
    /**
     * exportVo是否继承vo
     */
    private Boolean exportExtendsVo;

    /**
     * vo是否生成ResultMap
     */
    private Boolean voResultMap;

    /**
     * 排序字段map
     * 字段名 -> 是否倒序
     */
    private Map<String,Boolean> orderColumnMap =new HashMap<>();


    /**
     * 不对外爆露
     */
    private CustomConfig() { }


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
//        // 添加自定义字段
//        try {
//            for (Field field : this.getClass().getDeclaredFields()) {
//                String name = field.getName();
//                field.setAccessible(true);
//                data.put(name,field.get(this));
//            }
//        } catch (IllegalAccessException e) {
//            e.printStackTrace();
//        }
        // 当前时间
        data.put("nowTime", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

        // 时间类型列表
        List<JdbcType> jdbcTimeTypes =
                Arrays.asList(
                        JdbcType.DATE,
                        JdbcType.TIME,
                        JdbcType.TIMESTAMP,
                        JdbcType.DATETIMEOFFSET,// SQL Server 2008
                        JdbcType.TIME_WITH_TIMEZONE,// JDBC 4.2 JDK8
                        JdbcType.TIMESTAMP_WITH_TIMEZONE // JDBC 4.2 JDK8
                );
        // 对应fields[i].metaInfo.jdbcType
        data.put("jdbcTimeTypes",jdbcTimeTypes);


        List<TableField> fields = tableInfo.getFields();
        List<String> existColumnNames = fields.stream().map(TableField::getColumnName).collect(Collectors.toList());
        if (orderColumnMap !=null && orderColumnMap.size()>0){
            orderColumnMap.entrySet().stream()
                    .filter(e -> existColumnNames.contains(e.getKey()))
                    .map(e -> String.format("a.%s%s", e.getKey(), e.getValue() ? " desc" : ""))
                    .reduce((e1, e2) -> e1 + "," + e2)
                    .ifPresent(e->data.put("orderBySql",e))
            ;

        }



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
         * vo生成路径
         *
         * @param voPackage 签证官包
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:07
         */
        public Builder voPackage(@NotNull String voPackage){
            this.customConfig.voPackage=voPackage;
            return this;
        }

        /**
         * dto生成路径
         * @param dtoPackage
         * @return
         */
        public Builder dtoPackage(@NotNull String dtoPackage){
            this.customConfig.dtoPackage=dtoPackage;
            return this;
        }


        /**
         * 自定义文件
         *
         * @param customFile 自定义文件
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:08
         */
        public Builder customFile(@NotNull Map<String, String> customFile) {
            return customFile(customFile.entrySet().stream()
                    .map(e -> new CustomFile.Builder().fileName(e.getKey()).templatePath(e.getValue()).build())
                    .collect(Collectors.toList()));
        }

        /**
         * 自定义文件
         *
         * @param customFile 自定义文件
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:08
         */
        public Builder customFile(@NotNull CustomFile customFile) {
            this.customConfig.customFiles.add(customFile);
            return this;
        }

        /**
         * 自定义文件
         *
         * @param customFiles 自定义文件
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:08
         */
        public Builder customFile(@NotNull List<CustomFile> customFiles) {
            this.customConfig.customFiles.addAll(customFiles);
            return this;
        }

        /**
         * 清空自定义文件
         *
         * @param customFiles 自定义文件
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 16:08
         */
        public Builder clearCustomFile(@NotNull List<CustomFile> customFiles) {
            this.customConfig.customFiles.clear();
            return this;
        }


        /**
         * 自定义文件
         *
         * @param consumer 消费者
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:08
         */
        public Builder customFile(Consumer<CustomFile.Builder> consumer) {
            CustomFile.Builder builder = new CustomFile.Builder();
            consumer.accept(builder);
            this.customConfig.customFiles.add(builder.build());
            return this;
        }


        /**
         * vo继承实体类
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:16
         */
        public Builder voExtendsEntity(@NotNull Boolean b){
            this.customConfig.voExtendsEntity=b;
            return this;
        }

        /**
         * exportVo是否继承vo
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/23 15:42
         */
        public Builder exportExtendsVo(@NotNull Boolean b){
            this.customConfig.exportExtendsVo=b;
            return this;
        }

        /**
         * 是否创建voResultMap
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:16
         */
        public Builder voResultMap(@NotNull Boolean b){
            this.customConfig.voResultMap=b;
            return this;
        }

        /**
         * 添加自定义排序字段名称
         *
         * @param columnName 列名
         * @param isDesc     是否倒序
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:49
         */
        public Builder orderColumn(@NotNull String columnName , @NotNull Boolean isDesc){
            this.customConfig.orderColumnMap.put(columnName,isDesc);
            return this;
        }

        /**
         * 清空自定义排序字段名称
         *
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/17 15:51
         */
        public Builder clearOrderColumn(){
            this.customConfig.orderColumnMap.clear();
            return this;
        }

        /**
         * controller是否使用@RequestBody注解
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/23 22:01
         */
        public Builder requestBody(@NotNull Boolean b){
            this.customConfig.requestBody=b;
            return this;
        }


        /**
         * 是否添加校验注解
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @date 2023/07/23 22:06
         */
        public Builder enableValidated(@NotNull Boolean b){
            this.customConfig.enableValidated=b;
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
