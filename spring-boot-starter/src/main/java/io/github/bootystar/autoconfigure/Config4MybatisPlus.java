package io.github.bootystar.autoconfigure;

import com.baomidou.mybatisplus.autoconfigure.MybatisPlusAutoConfiguration;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.BlockAttackInnerInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.OptimisticLockerInnerInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;

/**
 * @author bootystar
 */
@Slf4j
@ConditionalOnClass({MybatisPlusAutoConfiguration.class, MybatisPlusInterceptor.class})
@AutoConfiguration(after = MybatisPlusAutoConfiguration.class)
public class Config4MybatisPlus {


    @Bean
    @ConditionalOnMissingBean(MybatisPlusInterceptor.class)
    @ConditionalOnBean(MybatisPlusAutoConfiguration.class)
    public MybatisPlusInterceptor mybatisPlusInterceptor() {
        log.debug("MybatisPlusInterceptor Configured");
        return new MybatisPlusInterceptor();
    }

    @ConditionalOnClass(PaginationInnerInterceptor.class)
    @ConditionalOnBean(MybatisPlusInterceptor.class)
    @Bean
    public String paginationInnerInterceptor(MybatisPlusInterceptor interceptor) {
        PaginationInnerInterceptor paginationInnerInterceptor = new PaginationInnerInterceptor();
        interceptor.addInnerInterceptor(paginationInnerInterceptor);
        log.debug("PaginationInnerInterceptor Configured");
        return null;
    }

    @ConditionalOnClass(OptimisticLockerInnerInterceptor.class)
    @ConditionalOnBean(MybatisPlusInterceptor.class)
    @Bean
    public String optimisticLockerInnerInterceptor(MybatisPlusInterceptor interceptor) {
        OptimisticLockerInnerInterceptor optimisticLockerInnerInterceptor = new OptimisticLockerInnerInterceptor();
        interceptor.addInnerInterceptor(optimisticLockerInnerInterceptor);
        log.debug("OptimisticLockerInnerInterceptor Configured");
        return null;
    }


    @ConditionalOnClass(BlockAttackInnerInterceptor.class)
    @ConditionalOnBean(MybatisPlusInterceptor.class)
    @Bean
    public String blockAttackInnerInterceptor(MybatisPlusInterceptor interceptor) {
        BlockAttackInnerInterceptor blockAttackInnerInterceptor = new BlockAttackInnerInterceptor();
        interceptor.addInnerInterceptor(blockAttackInnerInterceptor);
        log.debug("BlockAttackInnerInterceptor Configured");
        return null;
    }


}
