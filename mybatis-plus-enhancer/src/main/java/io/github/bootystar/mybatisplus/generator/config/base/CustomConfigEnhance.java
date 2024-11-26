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
     * 显示子类重写的方法
     */
    protected boolean enableServiceImplMethod = true;

    /**
     * 重写方法中显示注释
     */
    protected boolean enableServiceImplMethodComment = true;

    /**
     * 显示mapper方法
     */
    protected boolean enableMapperMethod = true;

    /**
     * 构造器
     *
     * @author bootystar
     */
    public static abstract class Builder<C extends CustomConfigEnhance, B extends CustomConfigEnhance.Builder<C, B>> extends CustomConfig.Builder<C, B> implements IConfigBuilder<C> {

        /**
         * 不生成service实现类的重写方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public B disableServiceImplOverrideMethod() {
            this.config.enableServiceImplMethod = false;
            return builder;
        }

        /**
         * 不生成service实现类的重写方法中的注释信息
         *
         * @return {@link B }
         * @author bootystar
         */
        public B disableServiceImplOverrideMethodComment() {
            this.config.enableServiceImplMethodComment = false;
            return builder;
        }


        /**
         * 不生成mapper的父类方法
         *
         * @return {@code U }
         * @author bootystar
         */
        public B disableMapperOverrideMethod() {
            this.config.enableMapperMethod = false;
            return builder;
        }




    }

}


