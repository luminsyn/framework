package io.github.bootystar.mybatisplus.generator.config;

import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import com.baomidou.mybatisplus.generator.util.ClassUtils;
import lombok.Getter;
import org.apache.ibatis.type.JdbcType;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author booty
 * @since 2023/9/15 14:49
 */
@Getter
public class ParentConfig implements IConfig {
    //--------------常量---------------
    public final String shift3="#";
    public final String shift4="$";
    public final String shift5="%";
    public final String shift8="*";
    public final String shiftLeft="{";
    public final String shiftRight="}";


    //--------------返回结果相关配置---------------

    /**
     * dto所在包
     */
    private String dtoPackage="dto";
    /**
     * vo所在包
     */
    private String voPackage="vo";

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

    // ------------------controller相关配置----------------


    /**
     * controller是否使用@RequestBody注解
     */
    private boolean requestBody;

    /**
     * 是否添加参数校验
     */
    private boolean enableValidated;
    /**
     * 是否添加跨域注解
     */
    private boolean enableOrigins;

    /**
     * 所有请求都使用post方法
     */
    private boolean allPost =false;
    /**
     * 请求基础url
     */
    private String baseUrl ;

    /**
     * 是否覆盖已有文件
     */
    private boolean fileOverride;

    /**
     * vo是否生成ResultMap
     */
    private Boolean voResultMap;

    /**
     * 新增排除的字段
     */
    private Collection<String> insertExcludeFields;

    /**
     * 修改排除的字段
     */
    private Collection<String> updateExcludeFields;

    /**
     * 排序字段map
     * 字段名 -> 是否倒序
     */
    private Map<String,Boolean> orderColumnMap;

    /**
     * java api包
     */
    private String javaApiPackage ="javax";

    /**
     * 新增dto
     */
    private Boolean generateInsert;
    /**
     * 更新dto
     */
    private Boolean generateUpdate;
    /**
     * 生成删除方法
     */
    private Boolean generateDelete;
    /**
     * 查询dto
     */
    private Boolean generateSelect;
    /**
     * 导入dto
     */
    private Boolean generateImport;
    /**
     * 导出dto
     */
    private Boolean generateExport;

    /**
     * 服务impl重写父类方法
     */
    private Boolean serviceImplOverride = true;


    /**
     * 自定义文件
     */
    private List<CustomFile> customFiles;



    @Override
    public Map<String, Object> renderData(TableInfo tableInfo) {
        HashMap<String, Object> data = new HashMap<>();
        // 添加自定义字段
        try {
            for (Field field : this.getClass().getDeclaredFields()) {
                String name = field.getName();
                field.setAccessible(true);
                if (!"customFiles".equals(field.getName())){
                    data.put(name,field.get(this));
                }
            }
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }

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
        // 排序字段
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
        return data;
    }

    public void setCustomFiles(List<CustomFile> customFiles) {
        this.customFiles = customFiles;
    }

    @Override
    public List<CustomFile> getCustomFiles() {
        return this.customFiles;
    }

    @Override
    public boolean getFileOverride() {
        return this.fileOverride;
    }




    public static class Builder implements IConfigBuilder<ParentConfig> {
        private final ParentConfig parentConfig;

        /**
         * 默认生成一个空的
         */
        public Builder() {
            this.parentConfig = new ParentConfig();
        }

        /**
         * 构建模板配置对象
         *
         * @return 模板配置对象
         */
        @Override
        public ParentConfig build() {
            return this.parentConfig;
        }




        /**
         * 使用jakarta的api
         * (自java17起移除了javax包,使用jakarta替代)
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/08/08 11:21
         */
        public ParentConfig.Builder jakartaApi(@NotNull Boolean b){
            if (b){
                this.parentConfig.javaApiPackage ="jakarta";
            }else{
                this.parentConfig.javaApiPackage ="javax";
            }
            return this;
        }

        /**
         * dto所在包
         *
         * @param packageName 包名
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/31 11:18
         */
        public ParentConfig.Builder dtoPackage(@NotNull String packageName){
            this.parentConfig.dtoPackage=packageName;
            return this;
        }

        /**
         * vo所在包
         *
         * @param packageName 包名
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/31 11:18
         */
        public ParentConfig.Builder voPackage(@NotNull String packageName){
            this.parentConfig.voPackage=packageName;
            return this;
        }

        /**
         * 返回结果类
         *
         * @param returnResultClass 返回结果类
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/13 16:12
         */
        public ParentConfig.Builder returnResultClass(Class<?> returnResultClass){
            if (returnResultClass==null){
                this.parentConfig.returnResultClass=null;
                this.parentConfig.returnResultClassPackage=null;
                this.parentConfig.returnResultGenericType=false;
                return this;
            }
            String fullClassName = returnResultClass.getName();
            this.parentConfig.returnResultClassPackage= fullClassName;
            this.parentConfig.returnResultClass= ClassUtils.getSimpleName(fullClassName);
            return this;
        }

        /**
         * 返回结果类
         *
         * @param fullClassName 返回结果类
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/13 16:12
         */
        public ParentConfig.Builder returnResultClass(String fullClassName){
            if (fullClassName==null){
                this.parentConfig.returnResultClass=null;
                this.parentConfig.returnResultClassPackage=null;
                this.parentConfig.returnResultGenericType=false;
                return this;
            }
            this.parentConfig.returnResultClassPackage= fullClassName;
            this.parentConfig.returnResultClass=ClassUtils.getSimpleName(fullClassName);
            return this;
        }


        /**
         * 返回结果是否为泛型类型
         *
         * @param isGenericType 是泛型类型
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/14 09:20
         */
        public ParentConfig.Builder returnResultGenericType(@NotNull Boolean isGenericType){
            this.parentConfig.returnResultGenericType=isGenericType;
            return this;
        }

        /**
         * 返回结果静态方法名字
         *
         * @param name 名字
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/14 09:20
         */
        public ParentConfig.Builder returnResultDefaultStaticMethodName(@NotNull String name){
            this.parentConfig.returnResultDefaultStaticMethodName=name;
            return this;
        }

        /**
         * 添加插入排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/14 15:38
         */
        public ParentConfig.Builder insertExcludeFields(List<String> fieldNames){
            this.parentConfig.insertExcludeFields = fieldNames;
            return this;
        }

        /**
         * 添加更新排除字段
         *
         * @param fieldNames 字段名称
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/14 15:38
         */
        public ParentConfig.Builder updateExcludeFields(List<String> fieldNames){
            this.parentConfig.updateExcludeFields = fieldNames;
            return this;
        }

        /**
         * controller是否使用@RequestBody注解
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/23 22:01
         */
        public ParentConfig.Builder requestBody(@NotNull Boolean b){
            this.parentConfig.requestBody=b;
            return this;
        }

        /**
         * 是否添加校验注解
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/23 22:06
         */
        public ParentConfig.Builder enableValidated(@NotNull Boolean b){
            this.parentConfig.enableValidated=b;
            return this;
        }


        /**
         * 是否创建voResultMap
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/17 15:16
         */
        public ParentConfig.Builder voResultMap(@NotNull Boolean b){
            this.parentConfig.voResultMap=b;
            return this;
        }

        /**
         * 排序字段map
         * k=字段名 v=启用倒序
         *
         * @param map 地图
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/31 09:12
         */
        public ParentConfig.Builder orderColumnMap(Map<String,Boolean> map){
            this.parentConfig.orderColumnMap=map;
            return this;
        }

        /**
         * 添加排序字段
         *
         * @param columnName 列名
         * @param isDesc     是desc
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/31 11:28
         */
        public ParentConfig.Builder orderColumn(@NotNull String columnName, @NotNull Boolean isDesc){
            if (this.parentConfig.orderColumnMap==null){
                this.parentConfig.orderColumnMap=new HashMap<>();
            }
            this.parentConfig.orderColumnMap.put(columnName,isDesc);
            return this;
        }



        /**
         * controller全部使用post请求
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/09/06 16:04
         */
        public ParentConfig.Builder allPost(@NotNull Boolean b){
            this.parentConfig.allPost=b;
            return this;
        }

        /**
         * 跨域注解
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/09/07 10:20
         */
        public ParentConfig.Builder enableOrigins(@NotNull Boolean b){
            this.parentConfig.enableOrigins=b;
            return this;
        }

        /**
         * controller请求前缀
         *
         * @param url url
         * @return {@code Builder }
         * @author booty
         * @since 2023/09/07 10:14
         */
        public ParentConfig.Builder baseUrl(@NotNull String url){
            if (url==null||url.length()==0){
                this.parentConfig.baseUrl=url;
                return this;
            }
            if (!url.startsWith("/")){
                url="/"+url;
            }
            if (url.endsWith("/")){
                url=url.substring(0,url.length()-1);
            }
            this.parentConfig.baseUrl=url;
            return this;
        }

        /**
         * 是否开启文件覆盖
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/09/18 09:37
         */
        public ParentConfig.Builder fileOverride(@NotNull Boolean b){
            this.parentConfig.fileOverride=b;
            return this;
        }


        /**
         * 生成新增方法
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/09/18 09:37
         */
        public ParentConfig.Builder generateInsert(@NotNull Boolean b){
            this.parentConfig.generateInsert=b;
            return this;
        }



        /**
         * 生成更新方法
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/09/18 09:36
         */
        public ParentConfig.Builder generateUpdate(@NotNull Boolean b){
            this.parentConfig.generateUpdate=b;
            return this;
        }

        /**
         * 生成查询方法
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/09/18 09:36
         */
        public ParentConfig.Builder generateSelect(@NotNull Boolean b){
            this.parentConfig.generateSelect=b;
            return this;
        }

        /**
         * 生成导出方法
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/09/18 09:36
         */
        public ParentConfig.Builder generateExport(@NotNull Boolean b){
            this.parentConfig.generateExport=b;
            return this;
        }

        /**
         * 生成导入方法
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/09/18 09:36
         */
        public ParentConfig.Builder generateImport(@NotNull Boolean b){
            this.parentConfig.generateImport=b;
            return this;
        }

        /**
         * 生成删除
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/10/23
         */
        public ParentConfig.Builder generateDelete(@NotNull Boolean b){
            this.parentConfig.generateDelete=b;
            return this;
        }


        /**
         * 服务impl重写父类方法
         *
         * @param b b
         * @return {@code ParentConfig.Builder }
         * @author booty
         * @since 2023/09/18 11:32
         */
        public ParentConfig.Builder serviceImplOverride(@NotNull Boolean b){
            this.parentConfig.serviceImplOverride=b;
            return this;
        }




    }


}
