package io.github.bootystar.mybatisplus.config.base;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import io.github.bootystar.mybatisplus.util.ReflectUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 配置构建器基类
 * @author bootystar
 */
@Slf4j
public abstract class ConfigBaseBuilder<T extends ConfigBase ,U> implements IConfigBuilder<T> {

    protected T config;
    protected U builder;

    public ConfigBaseBuilder() {
        config= initConfig();
        builder= initBuilder();
    }

    protected abstract T initConfig();
    protected abstract U initBuilder();

    /**
     * 构建模板配置对象
     *
     * @return 模板配置对象
     */
    @Override
    public T build() {
        return this.config;
    }
    
    
    //=============DTO VO =================

    /**
     * DTO所在包
     *
     * @param packageName 包名
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U DTOPackage(String packageName) {
        this.config.DTOPackage = packageName;
        return this.builder;
    }

    /**
     * VO所在包
     *
     * @param packageName 包名
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U VOPackage(String packageName) {
        this.config.VOPackage = packageName;
        return this.builder;
    }

    /**
     * 添加插入排除字段
     *
     * @param fieldNames 字段名称
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U insertExcludeFields(List<String> fieldNames) {
        this.config.insertExcludeFields = fieldNames;
        return this.builder;
    }

    /**
     * 添加更新排除字段
     *
     * @param fieldNames 字段名称
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U updateExcludeFields(List<String> fieldNames) {
        this.config.updateExcludeFields = fieldNames;
        return this.builder;
    }


    /**
     * 开启文件覆盖(vo及dto)
     *
     * @return {@code U }
     * @author bootystar
     *
     */
    public U enableFileOverride() {
        this.config.fileOverride = true;
        return this.builder;
    }



    /**
     * 不在vo上导出(生成额外ExportDTO)
     *
     * @return {@code U }
     * @author bootystar
     * @since 2023/12/19
     */
    public U disableExportOnVO() {
        this.config.exportOnVO = false;
        return this.builder;
    }

    /**
     * 不在vo上导入(生成额外ImportDTO)
     *
     * @return {@code U }
     * @author bootystar
     * @since 2023/12/19
     */
    public U disableImportOnVO() {
        this.config.importOnVO = false;
        return this.builder;
    }

    /**
     * vo上添加@Tablefield属性注释
     *
     * @return {@code U }
     * @author bootystar
     * @since 2023/12/19
     */
    public U enableFieldAnnotationOnVO() {
        this.config.fieldAnnotationOnVO = true;
        return this.builder;
    }
    
    
    

//===========================controller======================
    
    /**
     * controller请求前缀
     *
     * @param url url
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U baseUrl(String url) {
        if (url == null || url.length() == 0) {
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
     * controller请求固定post请求
     * 会关闭restful风格
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U enableAllPost() {
        this.config.allPost = true;
        return this.builder;
    }

    /**
     * 跨域注解
     *
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U enableOrigins() {
        this.config.enableOrigins =true;
        return this.builder;
    }

    /**
     * 使用jakarta的api
     * (自java17起移除了javax包,使用jakarta替代)
     *
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U enableJakartaApi() {
        this.config.javaApiPackage = "jakarta";
        return this.builder;
    }
    
    /**
     * 基础增删查改使用restful风格
     * (使用GET/POST/PUT/DELETE)
     * (enableAllPost后无效)
     *
     * @return {@code U }
     * @author bootystar
     * @since 2023/11/02
     */
    public U enableRestful() {
        this.config.restful = true;
        return this.builder;
    }
    

    /**
     * 禁用消息体接收数据
     * (不使用@RequestBody注解)
     *
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U disableRequestBody() {
        this.config.requestBody = false;
        return this.builder;
    }
    

    /**
     * 禁用参数校验注解
     *
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U disableValidated() {
        this.config.enableValidated = false;
        return this.builder;
    }


    /**
     * 指定controller的返回结果包装类及方法
     * 指定的方法需要接收{@link java.lang.Object} 作为参数
     * 建议静态方法或构造器,或返回自身的对象方法
     *
     * @param methodReference 方法引用
     * @return {@link U }
     * @author bootystar
     */
    public <R> U returnMethod(SFunction<Object, R> methodReference){
        ReflectUtil.LambdaMethod lambdaMethod = ReflectUtil.lambdaMethodInfo(methodReference,Object.class);
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
     * @return {@link U }
     * @author bootystar
     */
    public <O,R> U pageMethod(SFunction<IPage<O>, R> methodReference){
        ReflectUtil.LambdaMethod lambdaMethod = ReflectUtil.lambdaMethodInfo(methodReference,IPage.class);
        this.config.pageResultClassPackage = lambdaMethod.getClassPackage();
        this.config.pageResultClass = lambdaMethod.getClassSimpleName();
        this.config.pageResultMethodName = lambdaMethod.getMethodNameFullStr();
        this.config.pageResultGenericType = lambdaMethod.isGenericTypeClass();
        return this.builder;
    }
    
//==================mapper=======================

    /**
     * 创建VOResultMap字段映射
     *
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U enableResultMapForVO() {
        this.config.resultMapForVO = true;
        return this.builder;
    }

    /**
     * 排序字段map
     * k=字段名 v=启用倒序
     *
     * @param map 地图
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U orderColumnMap(Map<String, Boolean> map) {
        this.config.orderColumnMap = map;
        return this.builder;
    }

    /**
     * 添加排序字段
     *
     * @param columnName 列名
     * @param isDesc     是desc
     * @return {@code Builder }
     * @author bootystar
     *
     */
    public U orderColumn(String columnName, boolean isDesc) {
        if (this.config.orderColumnMap == null) {
            this.config.orderColumnMap = new HashMap<>();
        }
        if (columnName == null || columnName.length() == 0){
            return this.builder;
        }
        this.config.orderColumnMap.put(columnName, isDesc);
        return this.builder;
    }

    

    //============方法设置================

    /**
     * 不生成新增方法
     *
     * @return {@code U }
     * @author bootystar
     *
     */
    public U disableInsert() {
        this.config.generateInsert = false;
        return this.builder;
    }


    /**
     * 不生成更新方法
     *
     * @return {@code U }
     * @author bootystar
     *
     */
    public U disableUpdate() {
        this.config.generateUpdate = false;
        return this.builder;
    }

    /**
     * 不生成查询方法
     *
     * @return {@code U }
     * @author bootystar
     *
     */
    public U disableSelect() {
        this.config.generateSelect = false;
        return this.builder;
    }

    /**
     * 不生成导出方法
     *
     * @return {@code U }
     * @author bootystar
     *
     */
    public U disableExport() {
        this.config.generateExport = false;
        return this.builder;
    }

    /**
     * 不生成导入方法
     *
     * @return {@code U }
     * @author bootystar
     *
     */
    public U disableImport() {
        this.config.generateImport = false;
        return this.builder;
    }

    /**
     * 不生成删除方法
     *
     * @return {@code U }
     * @author bootystar
     * @since 2023/10/23
     */
    public U disableDelete() {
        this.config.generateDelete = false;
        return this.builder;
    }





   
    


}

