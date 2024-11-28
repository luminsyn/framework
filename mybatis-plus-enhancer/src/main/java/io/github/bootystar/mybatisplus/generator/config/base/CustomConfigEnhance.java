package io.github.bootystar.mybatisplus.generator.config.base;

import io.github.bootystar.mybatisplus.generator.info.ClassInfo;

/**
 * 继承增强配置项
 *
 * @author bootystar
 */
public abstract class CustomConfigEnhance extends CustomConfig {

    /**
     * 生成重写的方法
     */
    protected boolean overrideMethods = true;

    /**
     * mapper入参dto
     */
    protected ClassInfo mapperDTO = new ClassInfo();

    /**
     * 构造器
     *
     * @author bootystar
     */
    public static abstract class Builder<C extends CustomConfigEnhance, B extends CustomConfigEnhance.Builder<C, B>> extends CustomConfig.Builder<C, B> {

        /**
         * 不生成重写的方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public B disableOverrideMethods() {
            this.config.overrideMethods = false;
            return builder;
        }

    }

}


