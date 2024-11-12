package io.github.bootystar.mybatisplus.autoconfigure;

import com.baomidou.mybatisplus.autoconfigure.MybatisPlusProperties;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Configuration;

/**
 * @author bootystar
 */
@Slf4j
@Configuration
@ConditionalOnClass(MybatisPlusProperties.class)
public class SpringProp implements InitializingBean {





    @Override
    public void afterPropertiesSet() throws Exception {

    }


}
