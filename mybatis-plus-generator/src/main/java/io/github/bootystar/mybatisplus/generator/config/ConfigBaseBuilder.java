package io.github.bootystar.mybatisplus.generator.config;

import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import com.baomidou.mybatisplus.generator.util.ClassUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author booty
 *
 */
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

    /**
     * 使用jakarta的api
     * (自java17起移除了javax包,使用jakarta替代)
     *
     * @param b b
     * @return {@code Builder }
     * @author booty
     *
     */
    public U jakartaApi(boolean b) {
        if (b) {
            this.config.javaApiPackage = "jakarta";
        } else {
            this.config.javaApiPackage = "javax";
        }
        return this.builder;
    }

    /**
     * DTO所在包
     *
     * @param packageName 包名
     * @return {@code Builder }
     * @author booty
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
     * @author booty
     *
     */
    public U VOPackage(String packageName) {
        this.config.VOPackage = packageName;
        return this.builder;
    }

    /**
     * 返回结果类
     *
     * @param returnResultClass 返回结果类
     * @return {@code Builder }
     * @author booty
     *
     */
    public U returnResultClass(Class<?> returnResultClass) {
        if (returnResultClass == null) {
            this.config.returnResultClass = null;
            this.config.returnResultClassPackage = null;
            this.config.returnResultGenericType = false;
            return this.builder;
        }
        String fullClassName = returnResultClass.getName();
        this.config.returnResultClassPackage = fullClassName;
        this.config.returnResultClass = ClassUtils.getSimpleName(fullClassName);
        return this.builder;
    }

    /**
     * 返回结果类
     *
     * @param fullClassName 返回结果类
     * @return {@code Builder }
     * @author booty
     *
     */
    public U returnResultClass(String fullClassName) {
        if (fullClassName == null) {
            this.config.returnResultClass = null;
            this.config.returnResultClassPackage = null;
            this.config.returnResultGenericType = false;
            return this.builder;
        }
        this.config.returnResultClassPackage = fullClassName;
        this.config.returnResultClass = ClassUtils.getSimpleName(fullClassName);
        return this.builder;
    }


    /**
     * 返回结果是否为泛型类型
     *
     * @param isGenericType 是泛型类型
     * @return {@code Builder }
     * @author booty
     *
     */
    public U returnResultGenericType(boolean isGenericType) {
        this.config.returnResultGenericType = isGenericType;
        return this.builder;
    }

    /**
     * 返回结果静态方法名字
     *
     * @param name 名字
     * @return {@code Builder }
     * @author booty
     *
     */
    public U returnResultDefaultStaticMethodName(String name) {
        this.config.returnResultDefaultStaticMethodName = name;
        return this.builder;
    }

    /**
     * 添加插入排除字段
     *
     * @param fieldNames 字段名称
     * @return {@code Builder }
     * @author booty
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
     * @author booty
     *
     */
    public U updateExcludeFields(List<String> fieldNames) {
        this.config.updateExcludeFields = fieldNames;
        return this.builder;
    }

    /**
     * controller是否使用@RequestBody注解
     *
     * @param b b
     * @return {@code Builder }
     * @author booty
     *
     */
    public U requestBody(boolean b) {
        this.config.requestBody = b;
        return this.builder;
    }

    /**
     * 是否添加校验注解
     *
     * @param b b
     * @return {@code Builder }
     * @author booty
     *
     */
    public U enableValidated(boolean b) {
        this.config.enableValidated = b;
        return this.builder;
    }


    /**
     * 是否创建VOResultMap
     *
     * @param b b
     * @return {@code Builder }
     * @author booty
     *
     */
    public U resultMapForVO(boolean b) {
        this.config.resultMapForVO = b;
        return this.builder;
    }

    /**
     * 排序字段map
     * k=字段名 v=启用倒序
     *
     * @param map 地图
     * @return {@code Builder }
     * @author booty
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
     * @author booty
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


    /**
     * controller全部使用post请求
     *
     * @param b b
     * @return {@code Builder }
     * @author booty
     *
     */
    public U allPost(boolean b) {
        this.config.allPost = b;
        return this.builder;
    }

    /**
     * 跨域注解
     *
     * @param b b
     * @return {@code Builder }
     * @author booty
     *
     */
    public U enableOrigins(boolean b) {
        this.config.enableOrigins = b;
        return this.builder;
    }

    /**
     * controller请求前缀
     *
     * @param url url
     * @return {@code Builder }
     * @author booty
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
     * 是否开启文件覆盖
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U fileOverride(boolean b) {
        this.config.fileOverride = b;
        return this.builder;
    }


    /**
     * 生成新增方法
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U generateInsert(boolean b) {
        this.config.generateInsert = b;
        return this.builder;
    }


    /**
     * 生成更新方法
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U generateUpdate(boolean b) {
        this.config.generateUpdate = b;
        return this.builder;
    }

    /**
     * 生成查询方法
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U generateSelect(boolean b) {
        this.config.generateSelect = b;
        return this.builder;
    }

    /**
     * 生成导出方法
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U generateExport(boolean b) {
        this.config.generateExport = b;
        return this.builder;
    }

    /**
     * 生成导入方法
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U generateImport(boolean b) {
        this.config.generateImport = b;
        return this.builder;
    }

    /**
     * 生成删除
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U generateDelete(boolean b) {
        this.config.generateDelete = b;
        return this.builder;
    }


    /**
     * rest样式
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U restStyle(boolean b) {
        this.config.restStyle = b;
        return this.builder;
    }


    /**
     * 在vo上导出
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U exportOnVO(boolean b) {
        this.config.exportOnVO = b;
        return this.builder;
    }

    /**
     * 在vo上导入
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U importOnVO(boolean b) {
        this.config.importOnVO = b;
        return this.builder;
    }

    /**
     * vo上添加属性注释
     *
     * @param b b
     * @return {@code U }
     * @author booty
     *
     */
    public U fieldAnnotationOnVO(boolean b) {
        this.config.fieldAnnotationOnVO = b;
        return this.builder;
    }

}

