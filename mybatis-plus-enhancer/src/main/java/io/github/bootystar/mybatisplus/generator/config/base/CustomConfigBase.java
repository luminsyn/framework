package io.github.bootystar.mybatisplus.generator.config.base;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.util.ReflectHelper4MybatisPlus;
import lombok.Data;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.JdbcType;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 配置基类
 *
 * @author bootystar
 */
@Data
@Slf4j
public abstract class CustomConfigBase implements CustomConfig {

    /**
     * 自定义文件
     */
    @Setter
    protected List<CustomFile> customFiles;

    @Override
    public Map<String, Object> renderData(TableInfo tableInfo) {
        HashMap<String, Object> data = new HashMap<>();
        // 添加自定义字段
        try {
            Collection<Field> fields = ReflectHelper4MybatisPlus.fieldMap(getClass()).values();
            for (Field field : fields) {
                data.put(field.getName(), field.get(this));
            }
        } catch (IllegalAccessException e) {
            log.error("Generate Injection Field Error Please Report to Developer", e);
        }
        Set<String> importPackages = tableInfo.getImportPackages();
        Set<String> importPackages4DTO = new HashSet<>();
        for (String importPackage : importPackages) {
            if (!importPackage.startsWith("com.baomidou.mybatisplus.annotation")) {
                importPackages4DTO.add(importPackage);
            }
        }
        // 当前时间
        data.put("nowTime", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

        // 时间类型列表
        List<JdbcType> jdbcTimeTypes = Arrays.asList(JdbcType.DATE, JdbcType.TIME, JdbcType.TIMESTAMP, JdbcType.DATETIMEOFFSET,// SQL Server 2008
                JdbcType.TIME_WITH_TIMEZONE,// JDBC 4.2 JDK8
                JdbcType.TIMESTAMP_WITH_TIMEZONE // JDBC 4.2 JDK8
        );
        // 对应fields[i].metaInfo.jdbcType
        data.put("jdbcTimeTypes", jdbcTimeTypes);
        // 排序字段
        List<TableField> fields = tableInfo.getFields();
        List<String> existColumnNames = fields.stream().map(TableField::getColumnName).collect(Collectors.toList());
        if (orderColumnMap != null && !orderColumnMap.isEmpty()) {
            orderColumnMap.entrySet().stream().filter(e -> existColumnNames.contains(e.getKey())).map(e -> String.format("a.`%s`%s", e.getKey(), e.getValue() ? " DESC" : "")).reduce((e1, e2) -> e1 + " , " + e2).ifPresent(e -> data.put("orderBySql", e));
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
     * 是否覆盖已有文件
     */
    protected boolean fileOverride;

    //------------------DTO相关配置----------------

    /**
     * DTO所在包
     */
    protected String package4DTO = "dto";

    /**
     * 新增排除的字段
     */
    protected Collection<String> insertExcludeColumns;

    /**
     * 修改排除的字段
     */
    protected Collection<String> updateExcludeColumns;

    //------------------VO相关配置----------------

    /**
     * VO所在包
     */
    protected String package4VO = "vo";

    /**
     * 使用VO生出导出Excel(不生成额外ExportDTO)
     */
    protected boolean exportOnVO = true;

    /**
     * 使用VO生出导入Excel(不生成额外ImportDTO)
     */
    protected boolean importOnVO = true;

    /**
     * VO属性上添加{@link com.baomidou.mybatisplus.annotation.TableField}注解
     */
    protected boolean fieldAnnotationOnVO;

    //--------------返回结果相关配置---------------

    /**
     * 返回结果类所在包
     */
    protected String returnResultClassPackage;

    /**
     * 返回结果是否支持泛型
     */
    protected boolean returnResultGenericType;
    /**
     * 返回结果类
     */
    protected String returnResultClass;

    /**
     * 返回方法
     */
    protected String returnResultMethodName;

    /**
     * 返回结果类所在包
     */
    protected String pageResultClassPackage;

    /**
     * 返回结果是否支持泛型
     */
    protected boolean pageResultGenericType;
    /**
     * 返回结果类
     */
    protected String pageResultClass;

    /**
     * 返回方法
     */
    protected String pageResultMethodName;


    // ------------------controller相关配置----------------

    /**
     * controller是否使用@RequestBody注解
     */
    protected boolean requestBody = true;

    /**
     * java api包
     */
    protected String javaApiPackage = "javax";

    /**
     * 是否添加参数校验
     */
    protected boolean enableValidated = true;
    /**
     * 是否添加跨域注解
     */
    protected boolean enableOrigins;

    /**
     * 复杂查询使用post请求
     */
    protected boolean postOnComplicatedSelect = true;

    /**
     * 请求基础url
     */
    protected String baseUrl;

    /**
     * restful样式
     */
    protected boolean restful = true;


    // ------------------mapper相关配置----------------


    /**
     * 排序字段map
     * 字段名 -> 是否倒序
     */
    protected Map<String, Boolean> orderColumnMap;

    /**
     * VO是否生成ResultMap
     */
    protected boolean resultMapForVO;

    //   ------------------ 生成相关配置----------------

    /**
     * 新增DTO
     */
    protected boolean generateInsert = true;
    /**
     * 更新DTO
     */
    protected boolean generateUpdate = true;
    /**
     * 生成删除方法
     */
    protected boolean generateDelete = true;
    /**
     * 查询DTO
     */
    protected boolean generateSelect = true;
    /**
     * 导入DTO
     */
    protected boolean generateImport = true;
    /**
     * 导出DTO
     */
    protected boolean generateExport = true;
    /**
     * 生成查询dto
     */
    protected boolean generateSelectDTO = true;


    /**
     * 构建器
     *
     * @author bootystar
     */
    @SuppressWarnings("unused")
    public static abstract class Builder<C extends CustomConfigBase, B extends IConfigBuilder<C>> implements IConfigBuilder<C> {

        protected C config;

        protected B builder;

        public Builder() {
            config = initConfig();
            builder = initBuilder();
        }

        protected abstract C initConfig();

        protected abstract B initBuilder();

        /**
         * 构建模板配置对象
         *
         * @return 模板配置对象
         * @author bootystar
         */
        @Override
        public C build() {
            return this.config;
        }

        //==================DTO VO=======================

        /**
         * DTO所在包
         *
         * @param packageName 包名
         * @return this
         * @author bootystar
         */
        public B DTOPackage(String packageName) {
            this.config.package4DTO = packageName;
            return this.builder;
        }

        /**
         * VO所在包
         *
         * @param packageName 包名
         * @return this
         * @author bootystar
         */
        public B VOPackage(String packageName) {
            this.config.package4VO = packageName;
            return this.builder;
        }

        /**
         * 添加插入排除字段
         *
         * @param columns 字段名称
         * @return this
         * @author bootystar
         */
        public B insertExcludeColumns(List<String> columns) {
            this.config.insertExcludeColumns = columns;
            return this.builder;
        }

        /**
         * 添加更新排除字段
         *
         * @param columns 字段名称
         * @return this
         * @author bootystar
         */
        public B updateExcludeColumns(List<String> columns) {
            this.config.updateExcludeColumns = columns;
            return this.builder;
        }

        /**
         * 开启文件覆盖(vo及dto)
         *
         * @return {@link B }
         * @author bootystar
         */
        public B enableFileOverride() {
            this.config.fileOverride = true;
            return this.builder;
        }

        /**
         * 不在vo上导出(生成额外ExportDTO)
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableExportOnVO() {
            this.config.exportOnVO = false;
            return this.builder;
        }

        /**
         * 不在vo上导入(生成额外ImportDTO)
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableImportOnVO() {
            this.config.importOnVO = false;
            return this.builder;
        }

        /**
         * VO属性上添加{@link com.baomidou.mybatisplus.annotation.TableField}注解
         *
         * @return {@link B }
         * @author bootystar
         */
        public B enableFieldAnnotationOnVO() {
            this.config.fieldAnnotationOnVO = true;
            return this.builder;
        }

        //==================controller=======================

        /**
         * controller请求前缀
         *
         * @param url url
         * @return this
         * @author bootystar
         */
        public B baseUrl(String url) {
            if (url == null || url.isEmpty()) {
                this.config.baseUrl = url;
                return this.builder;
            }
            if (!url.startsWith("/")) {
                url = "/" + url;
            }
            if (url.endsWith("/")) {
                url = url.substring(0, url.length() - 1);
            }
            this.config.baseUrl = url;
            return this.builder;
        }

        /**
         * 跨域注解
         *
         * @return this
         * @author bootystar
         */
        public B enableOrigins() {
            this.config.enableOrigins = true;
            return this.builder;
        }

        /**
         * 使用jakarta的api
         * (自java17起移除了javax包,使用jakarta替代)
         *
         * @return this
         * @author bootystar
         */
        public B enableJakartaApi() {
            this.config.javaApiPackage = "jakarta";
            return this.builder;
        }

        /**
         * 指定controller的返回结果包装类及方法
         *
         * @param methodReference 方法引用
         * @return {@link B }
         * @author bootystar
         */
        public <R> B returnMethod(SFunction<Object, R> methodReference) {
            ReflectHelper4MybatisPlus.LambdaMethod lambdaMethod = ReflectHelper4MybatisPlus.lambdaMethodInfo(methodReference, Object.class);
            this.config.returnResultClassPackage = lambdaMethod.getClassPackage();
            this.config.returnResultClass = lambdaMethod.getClassSimpleName();
            this.config.returnResultMethodName = lambdaMethod.getMethodNameFullStr();
            this.config.returnResultGenericType = lambdaMethod.isGenericTypeClass();
            return this.builder;
        }

        /**
         * 指定controller返回的分页包装类及方法
         * 指定的方法需要接收{@link com.baomidou.mybatisplus.core.metadata.IPage} 作为参数
         * 若未指定方法不为静态或构造器或返回自身的方法,会使用{@link com.baomidou.mybatisplus.core.metadata.IPage}替代
         *
         * @param methodReference 方法参考
         * @return {@link B }
         * @author bootystar
         */
        public <O, R> B pageMethod(SFunction<IPage<O>, R> methodReference) {
            ReflectHelper4MybatisPlus.LambdaMethod lambdaMethod = ReflectHelper4MybatisPlus.lambdaMethodInfo(methodReference, IPage.class);
            this.config.pageResultClassPackage = lambdaMethod.getClassPackage();
            this.config.pageResultClass = lambdaMethod.getClassSimpleName();
            this.config.pageResultMethodName = lambdaMethod.getMethodNameFullStr();
            this.config.pageResultGenericType = lambdaMethod.isGenericTypeClass();
            return this.builder;
        }

        /**
         * 禁止基础增删查改使用restful风格
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableRestful() {
            this.config.restful = false;
            return this.builder;
        }

        /**
         * 禁用消息体接收数据
         *
         * @return this
         * @author bootystar
         */
        public B disableRequestBody() {
            this.config.requestBody = false;
            return this.builder;
        }

        /**
         * 禁用参数校验注解
         *
         * @return this
         * @author bootystar
         */
        public B disableValidated() {
            this.config.enableValidated = false;
            return this.builder;
        }

        /**
         * 禁止多条件复杂查询使用post请求
         *
         * @return this
         * @author bootystar
         */
        public B disablePostOnComplicatedSelect() {
            this.config.postOnComplicatedSelect = false;
            return this.builder;
        }

        //==================mapper=======================

        /**
         * 创建VOResultMap字段映射
         *
         * @return this
         * @author bootystar
         */
        public B enableResultMapForVO() {
            this.config.resultMapForVO = true;
            return this.builder;
        }

        /**
         * 排序字段map
         * k=字段名 v=启用倒序
         * 如需清空,传入new HashMap<>()或null即可清空
         *
         * @param map 地图
         * @return this
         * @author bootystar
         */
        public B orderColumnMap(Map<String, Boolean> map) {
            this.config.orderColumnMap = map;
            return this.builder;
        }

        /**
         * 添加排序字段(不会清空已添加的)
         *
         * @param columnName 列名
         * @param isDesc     是desc
         * @return this
         * @author bootystar
         */
        public B orderColumn(String columnName, boolean isDesc) {
            if (this.config.orderColumnMap == null) {
                this.config.orderColumnMap = new HashMap<>();
            }
            if (columnName == null || columnName.isEmpty()) {
                return this.builder;
            }
            this.config.orderColumnMap.put(columnName, isDesc);
            return this.builder;
        }


        //============方法设置================

        /**
         * 不生成新增方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableInsert() {
            this.config.generateInsert = false;
            return this.builder;
        }


        /**
         * 不生成更新方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableUpdate() {
            this.config.generateUpdate = false;
            return this.builder;
        }

        /**
         * 不生成查询方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableSelect() {
            this.config.generateSelect = false;
            return this.builder;
        }

        /**
         * 不生成导出方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableExport() {
            this.config.generateExport = false;
            return this.builder;
        }

        /**
         * 不生成导入方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableImport() {
            this.config.generateImport = false;
            return this.builder;
        }

        /**
         * 不生成删除方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableDelete() {
            this.config.generateDelete = false;
            return this.builder;
        }
    }
    
}
