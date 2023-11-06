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
 * 自定义配置
 * 模板路径配置项
 *
 * @author booty
 * @since 2023/07/13 13:59
 * @since 2017-06-17
 */
@Getter
public class CrudConfig implements IConfig {

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

    //--------------controller基础配置---------------

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
    private Collection<String> insertExcludeFields = new HashSet<>();

    /**
     * 修改排除的字段
     */
    private Collection<String> updateExcludeFields = new HashSet<>();

    //--------------分页查询配置---------------

    /**
     * 生成分页扩展查询方法
     */
    private Boolean pageByDto;

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
     * vo是否继承entity
     */
    private Boolean voExtendsEntity;

    /**
     * 生成excel导出方法（基于分页扩展查询方法）
     */
    private Boolean exportExcel;

    /**
     * 导出实体类是否继承vo
     */
    private Boolean exportExtendsVo;


    //--------------excel导入配置---------------
    /**
     * 是否开启导入excel
     */
    private Boolean importExcel;

    /**
     * 导入excel类是否继承实体类
     */
    private Boolean importExtendsEntity;

    // --------------后续迭代--------------

    /**
     * 自定义文件
     */
    private List<CustomFile> customFiles;

    /**
     * java api包
     */
    private String javaApiPackage ="javax";
    /**
     * 所有请求都使用post方法
     */
    private Boolean allPost =false;
    /**
     * 请求基础url
     */
    private String baseUrl = null;

    /**
     * 是否覆盖已有文件
     */
    private Boolean fileOverride = false;

    /**
     * rest样式
     */
    private Boolean restStyle = false;



    /**
     * 不对外暴露
     */
    private CrudConfig() { }


    public void setCustomFiles(List<CustomFile> customFiles) {
        this.customFiles = customFiles;
    }

    /**
     * 呈现数据
     *
     * @param tableInfo 表信息
     * @return {@code Map<String, Object> }
     * @author booty
     * @since 2023/07/13 14:01
     */
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

    @Override
    public List<CustomFile> getCustomFiles() {
        return this.customFiles;
    }

    @Override
    public boolean getFileOverride() {
        return this.fileOverride;
    }

    /**
     * 模板路径配置构建者
     *
     * @author booty 3.5.3
     */
    public static class Builder implements IConfigBuilder<CrudConfig> {
        private final CrudConfig customConfig;

        /**
         * 默认生成一个空的
         */
        public Builder() {
            this.customConfig = new CrudConfig();
        }

        /**
         * 构建模板配置对象
         *
         * @return 模板配置对象
         */
        @Override
        public CrudConfig build() {
            return this.customConfig;
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
        public Builder jakartaApi(@NotNull Boolean b){
            if (b){
                this.customConfig.javaApiPackage ="jakarta";
            }else{
                this.customConfig.javaApiPackage ="javax";
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
        public Builder dtoPackage(@NotNull String packageName){
            this.customConfig.dtoPackage=packageName;
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
        public Builder voPackage(@NotNull String packageName){
            this.customConfig.voPackage=packageName;
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
        public Builder returnResultClass(Class<?> returnResultClass){
            if (returnResultClass==null){
                this.customConfig.returnResultClass=null;
                this.customConfig.returnResultClassPackage=null;
                this.customConfig.returnResultGenericType=false;
                return this;
            }
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
         * @since 2023/07/13 16:12
         */
        public Builder returnResultClass(String fullClassName){
            if (fullClassName==null){
                this.customConfig.returnResultClass=null;
                this.customConfig.returnResultClassPackage=null;
                this.customConfig.returnResultGenericType=false;
                return this;
            }
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
         * @since 2023/07/14 09:20
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
         * @since 2023/07/14 09:20
         */
        public Builder returnResultDefaultStaticMethodName(@NotNull String name){
            this.customConfig.returnResultDefaultStaticMethodName=name;
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
        public Builder insertExcludeFields(List<String> fieldNames){
            this.customConfig.insertExcludeFields = fieldNames;
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
        public Builder updateExcludeFields(List<String> fieldNames){
            this.customConfig.updateExcludeFields = fieldNames;
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
         * @since 2023/07/23 22:06
         */
        public Builder enableValidated(@NotNull Boolean b){
            this.customConfig.enableValidated=b;
            return this;
        }

        /**
         * 生成分页查询方法
         *
         * @param b 生成
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/14 10:04
         */
        public Builder pageByDto(@NotNull Boolean b){
            this.customConfig.pageByDto=b;
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
        public Builder voResultMap(@NotNull Boolean b){
            this.customConfig.voResultMap=b;
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
        public Builder orderColumnMap(Map<String,Boolean> map){
            this.customConfig.orderColumnMap=map;
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
        public Builder orderColumn(@NotNull String columnName,@NotNull Boolean isDesc){
            if (this.customConfig.orderColumnMap==null){
                this.customConfig.orderColumnMap=new HashMap<>();
            }
            this.customConfig.orderColumnMap.put(columnName,isDesc);
            return this;
        }


        /**
         * vo继承实体类
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/17 15:16
         */
        public Builder voExtendsEntity(@NotNull Boolean b){
            this.customConfig.voExtendsEntity=b;
            return this;
        }

        /**
         * 生成excel导出方法
         *
         * @param b 生成
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/14 10:03
         */
        public Builder exportExcel(@NotNull Boolean b){
            this.customConfig.exportExcel=b;
            return this;
        }

        /**
         * exportVo是否继承vo
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/23 15:42
         */
        public Builder exportExtendsVo(@NotNull Boolean b){
            this.customConfig.exportExtendsVo=b;
            return this;
        }


        /**
         * 生成导入excel方法
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/31 10:20
         */
        public Builder importExcel(@NotNull Boolean b){
            this.customConfig.importExcel=b;
            return this;
        }

        /**
         * 导入类是否继承entity
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/07/31 10:21
         */
        public Builder importExtendsEntity(@NotNull Boolean b){
            this.customConfig.importExtendsEntity=b;
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
        public Builder allPost(@NotNull Boolean b){
            this.customConfig.allPost=b;
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
        public Builder enableOrigins(@NotNull Boolean b){
            this.customConfig.enableOrigins=b;
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
        public Builder baseUrl(@NotNull String url){
            if (url==null||url.length()==0){
                this.customConfig.baseUrl=url;
                return this;
            }
            if (!url.startsWith("/")){
                url="/"+url;
            }
            if (url.endsWith("/")){
                url=url.substring(0,url.length()-1);
            }
            this.customConfig.baseUrl=url;
            return this;
        }

        /**
         * 是否开启文件覆盖
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/09/07 10:28
         */
        public Builder fileOverride(@NotNull Boolean b){
            this.customConfig.fileOverride=b;
            return this;
        }

        /**
         * rest样式
         *
         * @param b b
         * @return {@code Builder }
         * @author booty
         * @since 2023/11/02
         */
        public Builder restStyle(@NotNull Boolean b){
            this.customConfig.restStyle=b;
            return this;
        }

    }
}
