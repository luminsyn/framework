package io.github.bootystar.mybatisplus.generator.config.base;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.logic.common.MethodInfo;
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
        data.put("importPackages4DTO", importPackages4DTO);
        // 当前时间
        data.put("nowTime", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

        // 时间类型列表
        List<JdbcType> jdbcTimeTypes = Arrays.asList(
                JdbcType.DATE,
                JdbcType.TIME,
                JdbcType.TIMESTAMP,
                JdbcType.DATETIMEOFFSET,// SQL Server 2008
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
     * 新增或修改时排除的字段
     */
    protected Collection<String> editExcludeColumns;

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


    // ------------------controller相关配置----------------

    /**
     * 返回结果方法
     */
    protected MethodInfo returnMethod;

    /**
     * 分页结果方法
     */
    protected MethodInfo pageMethod;

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

    // ------------------ 生成相关配置----------------

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
         * 新增或修改时排除的字段
         *
         * @param columns 字段名称
         * @return this
         * @author bootystar
         */
        public B editExcludeColumns(String... columns) {
            this.config.editExcludeColumns = Arrays.asList(columns);
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
         * <p>
         * 指定的方法需要支持泛型或接收{@link java.lang.Object} 作为参数
         *
         * @param methodReference 方法引用
         * @return {@link B }
         * @author bootystar
         */
        public <R> B returnMethod(SFunction<Object, R> methodReference) {
            this.config.returnMethod = ReflectHelper4MybatisPlus.lambdaMethodInfo(methodReference, Object.class);
            return this.builder;
        }

        /**
         * 指定controller返回的分页包装类及方法
         * <p>
         * 指定的方法需要支持泛型或接收{@link com.baomidou.mybatisplus.core.metadata.IPage} 作为参数
         *
         * @param methodReference 方法参考
         * @return {@link B }
         * @author bootystar
         */
        public <O, R> B pageMethod(SFunction<IPage<O>, R> methodReference) {
            this.config.pageMethod = ReflectHelper4MybatisPlus.lambdaMethodInfo(methodReference, IPage.class);
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
         * 清空排序字段
         *
         * @return {@link B }
         * @author bootystar
         */
        public B sortColumnClear() {
            this.config.orderColumnMap.clear();
            return this.builder;
        }

        /**
         * 添加排序字段,越先添加优先级越高
         *
         * @param columnName 字段名
         * @param isDesc     是否倒排
         * @return this
         * @author bootystar
         */
        public B sortColumn(String columnName, boolean isDesc) {
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
         * 不生成删除方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableDelete() {
            this.config.generateDelete = false;
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

    }
    
}
