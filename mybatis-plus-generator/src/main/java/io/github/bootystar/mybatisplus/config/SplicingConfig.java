package io.github.bootystar.mybatisplus.config;

import io.github.bootystar.mybatisplus.config.base.ConfigBase;
import io.github.bootystar.mybatisplus.config.base.ConfigBaseBuilder;
import io.github.bootystar.mybatisplus.logic.splicing.dto.Splicer;
import lombok.Getter;

/**
 * SQL拼接配置类
 *
 * @author bootystar
 */
@Getter
public class SplicingConfig extends ConfigBase {

    public SplicingConfig() {
        super(SplicingConfig.class);
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
     * 拼接器类名称
     */
    protected String splicerClassSimpleName = Splicer.class.getSimpleName();

    /**
     * 拼接器全限定类名
     */
    protected String splicerClassFullName = Splicer.class.getName();

    /**
     * 使用拼接器
     */
    protected boolean splicer = true;

    /**
     * 构造器
     *
     * @author bootystar
     */
    public static class Builder extends ConfigBaseBuilder<SplicingConfig, Builder> {

        @Override
        protected SplicingConfig initConfig() {
            return new SplicingConfig();
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


