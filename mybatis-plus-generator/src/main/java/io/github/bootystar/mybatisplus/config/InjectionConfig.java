package io.github.bootystar.mybatisplus.config;

import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.config.base.ConfigBase;
import io.github.bootystar.mybatisplus.config.base.ConfigBaseBuilder;
import io.github.bootystar.mybatisplus.logic.injection.dto.InjectionDTO;
import io.github.bootystar.mybatisplus.logic.injection.dto.SortDTO;
import lombok.Getter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 父类生成器配置类
 *
 * @author bootystar
 */
@Getter
public class InjectionConfig extends ConfigBase {

    public InjectionConfig() {
        super(3);
    }


    /**
     * 显示 service impl方法
     */
    protected boolean showServiceImplMethod = true;

    /**
     * 显示mapper方法
     */
    protected boolean showMapperMethod = true;

    /**
     * 注入器类名称
     */
    protected String injectionClassSimpleName = InjectionDTO.class.getSimpleName();

    /**
     * 注入器全限定类名
     */
    protected String injectionClassFullName = InjectionDTO.class.getName();

    /**
     * 排序字段名称
     */
    protected String sortClassSimpleName = SortDTO.class.getSimpleName();

    /**
     * 排序字段名称
     */
    protected String sortClassFullName = SortDTO.class.getName();

    /**
     * 使用注入器
     */
    protected boolean injection = true;

    /**
     * 构造器
     * @author bootystar
     * @since 2023/12/19
     */
    public static class Builder extends ConfigBaseBuilder<InjectionConfig, Builder> {

        @Override
        protected InjectionConfig initConfig() {
            return new InjectionConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }


        /**
         * 不生成服务impl的父类方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public Builder disableServiceImplOverrideMethod() {
            this.config.showServiceImplMethod = false;
            return this;
        }

        /**
         * 不生成mapper的父类方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public Builder disableMapperOverrideMethod() {
            this.config.showMapperMethod = false;
            return this;
        }
    }

}


