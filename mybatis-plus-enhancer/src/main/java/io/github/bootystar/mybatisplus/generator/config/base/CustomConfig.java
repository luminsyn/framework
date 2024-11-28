package io.github.bootystar.mybatisplus.generator.config.base;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.builder.ConfigBuilder;
import com.baomidou.mybatisplus.generator.config.builder.CustomFile;
import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.generator.info.ClassInfo;
import io.github.bootystar.mybatisplus.generator.info.MethodInfo;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.type.JdbcType;

import java.io.File;
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
@Slf4j
public abstract class CustomConfig {

    public List<CustomFile> customFiles(ConfigBuilder config, TableInfo tableInfo) {
        List<CustomFile> customFiles = new ArrayList<>(8);
        String pathUnderParent4DTO = package4DTO.replaceAll("\\.", "\\" + File.separator);
        String pathUnderParent4VO = package4VO.replaceAll("\\.", "\\" + File.separator);
        String parentPath = config.getPathInfo().get(OutputFile.parent);
        String entityName = tableInfo.getEntityName();
        String path4DTO = parentPath + File.separator + pathUnderParent4DTO + File.separator;
        String path4VO = parentPath + File.separator + pathUnderParent4VO + File.separator;

        if (generateInsert || generateImport) {
            String fileName = "InsertDTO.java";
            String path = path4DTO + entityName + fileName;
            CustomFile.Builder builder = new CustomFile.Builder()
                    .fileName(fileName)
                    .filePath(path)
                    .templatePath("/templates/base/entityInsertDTO.java.vm")
                    .packageName(pathUnderParent4DTO);
            if (fileOverride) {
                builder.enableFileOverride();
            }
            customFiles.add(builder.build());
        }

        if (generateUpdate) {
            String fileName = "UpdateDTO.java";
            String path = path4DTO + entityName + fileName;
            CustomFile.Builder builder = new CustomFile.Builder()
                    .fileName(fileName)
                    .filePath(path)
                    .templatePath("/templates/base/entityUpdateDTO.java.vm")
                    .packageName(pathUnderParent4DTO);
            if (fileOverride) {
                builder.enableFileOverride();
            }
            customFiles.add(builder.build());
        }

        if ((generateSelect || generateExport) && selectDTO == null) {
            String fileName = "SelectDTO.java";
            String path = path4DTO + File.separator + entityName + fileName;
            CustomFile.Builder builder = new CustomFile.Builder()
                    .fileName(fileName)
                    .filePath(path)
                    .templatePath("/templates/base/entitySelectDTO.java.vm")
                    .packageName(pathUnderParent4DTO);
            if (fileOverride) {
                builder.enableFileOverride();
            }
            customFiles.add(builder.build());
        }
//        if (generateExport && !exportOnVO) {
//            String fileName = "ExportDTO.java";
//            String path = path4DTO + File.separator + entityName + fileName;
//            CustomFile.Builder builder = new CustomFile.Builder()
//                    .fileName(fileName)
//                    .filePath(path)
//                    .templatePath("/templates/base/entityExportDTO.java.vm")
//                    .packageName(pathUnderParent4DTO);
//            if (fileOverride) {
//                builder.enableFileOverride();
//            }
//            customFiles.add(builder.build());
//
//        }
//        if (generateImport && !importOnVO) {
//            String fileName = "ImportDTO.java";
//            String path = path4DTO + File.separator + entityName + fileName;
//            CustomFile.Builder builder = new CustomFile.Builder()
//                    .fileName(fileName)
//                    .filePath(path)
//                    .templatePath("/templates/base/entityImportDTO.java.vm")
//                    .packageName(pathUnderParent4DTO);
//            if (fileOverride) {
//                builder.enableFileOverride();
//            }
//            customFiles.add(builder.build());
//        }

        String fileName = "VO.java";
        String path = path4VO + File.separator + entityName + fileName;
        CustomFile.Builder builder = new CustomFile.Builder()
                .fileName(fileName)
                .filePath(path)
                .templatePath("/templates/base/entityVO.java.vm")
                .packageName(pathUnderParent4VO);
        if (fileOverride) {
            builder.enableFileOverride();
        }
        customFiles.add(builder.build());
        return customFiles;
    }

    public Map<String, Object> renderData(TableInfo tableInfo) {
        HashMap<String, Object> data = new HashMap<>();
        // 添加自定义字段
        try {
            Collection<Field> fields = MybatisPlusReflectHelper.fieldMap(getClass()).values();
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
            orderColumnMap.entrySet().stream()
                    .filter(e -> existColumnNames.contains(e.getKey()))
                    .map(e -> String.format("a.%s%s", e.getKey(), e.getValue() ? " DESC" : ""))
                    .reduce((e1, e2) -> e1 + " , " + e2)
                    .ifPresent(e -> data.put("orderBySql", e));
        }
        return data;
    }

    /**
     * 是否覆盖已有文件
     */
    protected boolean fileOverride;

    //------------------额外类相关配置----------------

    /**
     * 实体查询dto
     */
    protected ClassInfo selectDTO;

    /**
     * DTO所在包
     */
    protected String package4DTO = "dto";

    /**
     * VO所在包
     */
    protected String package4VO = "vo";

    /**
     * 新增或修改时排除的字段
     */
    protected Collection<String> editExcludeColumns;

    // ------------------controller相关配置----------------

    /**
     * 返回结果方法
     */
    protected MethodInfo returnMethod = new MethodInfo();

    /**
     * 分页结果方法
     */
    protected MethodInfo pageMethod = new MethodInfo();

    /**
     * controller是否使用@RequestBody注解
     */
    protected boolean requestBody = true;

    /**
     * servlet api包
     */
    protected String javaApiPackage = "javax";

    /**
     * 参数校验注解
     */
    protected boolean validated = true;
    /**
     * 跨域注解
     */
    protected boolean crossOrigins;

    /**
     * 复杂查询使用post请求
     */
    protected boolean postQuery = true;

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
    protected Map<String, Boolean> orderColumnMap = new LinkedHashMap<>();

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
     * 构建器
     *
     * @author bootystar
     */
    @SuppressWarnings("unused")
    public static abstract class Builder<C extends CustomConfig, B extends CustomConfig.Builder<C, B>> {

        protected C config;

        protected B builder;

        public Builder() {
            config = initConfig();
            builder = initBuilder();
        }

        /**
         * 构建模板配置对象
         *
         * @return 模板配置对象
         * @author bootystar
         */
        public C build() {
            return this.config;
        }

        protected abstract C initConfig();

        protected abstract B initBuilder();


        //==================DTO VO=======================

        /**
         * 使用Map作为查询DTO
         *
         * @return {@link B }
         * @author bootystar
         */
        public B mapAsSelectDTO() {
            this.config.selectDTO = new ClassInfo(Map.class);
            return this.builder;
        }

        /**
         * DTO所在包
         *
         * @param packageName 包名
         * @return this
         * @author bootystar
         */
        public B package4DTO(String packageName) {
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
        public B package4VO(String packageName) {
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
        public B enableCrossOrigins() {
            this.config.crossOrigins = true;
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
            this.config.returnMethod = MybatisPlusReflectHelper.lambdaMethodInfo(methodReference, Object.class);
            return this.builder;
        }

        /**
         * 指定controller返回的分页包装类及方法
         *
         * @param methodReference 方法参考
         * @return {@link B }
         * @author bootystar
         */
        public <O, R> B pageMethod(SFunction<IPage<O>, R> methodReference) {
            this.config.pageMethod = MybatisPlusReflectHelper.lambdaMethodInfo(methodReference, IPage.class);
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
            this.config.validated = false;
            return this.builder;
        }

        /**
         * 禁止多条件复杂查询使用post请求
         *
         * @return this
         * @author bootystar
         */
        public B disablePostQuery() {
            this.config.postQuery = false;
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
         * 不生成导出方法
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableExport() {
            this.config.generateExport = false;
            return this.builder;
        }



    }

}
