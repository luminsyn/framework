package io.github.bootystar.mybatisplus.generator.config.base;

import com.baomidou.mybatisplus.generator.config.IConfigBuilder;
import lombok.Getter;

/**
 * 继承增强配置项
 *
 * @author bootystar
 */
@Getter
public abstract class CustomConfigEnhance extends CustomConfig {

    /**
     * 显示 service impl方法
     */
    protected boolean showServiceImplMethod = true;

    /**
     * 显示mapper方法
     */
    protected boolean showMapperMethod = true;

    /**
     * 构造器
     *
     * @author bootystar
     */
    public static abstract class Builder<C extends CustomConfigEnhance, B extends CustomConfigEnhance.Builder<C, B>> extends CustomConfig.Builder<C, B> implements IConfigBuilder<C> {

        /**
         * 不生成服务impl的父类方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public B disableServiceImplOverrideMethod() {
            this.config.showServiceImplMethod = false;
            return builder;
        }

        /**
         * 不生成mapper的父类方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public B disableMapperOverrideMethod() {
            this.config.showMapperMethod = false;
            return builder;
        }
    }

}


