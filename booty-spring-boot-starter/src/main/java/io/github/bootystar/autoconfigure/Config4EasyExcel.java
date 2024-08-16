package io.github.bootystar.autoconfigure;

import com.alibaba.excel.converters.DefaultConverterLoader;
import io.github.bootystar.autoconfigure.prop.EasyExcelProp;
import io.github.bootystar.helper.easyexcel.EasyExcelConverterTool;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author booty
 */

@Slf4j
@Configuration
@EnableConfigurationProperties(EasyExcelProp.class)
@ConditionalOnClass({EasyExcelConverterTool.class, DefaultConverterLoader.class})
public class Config4EasyExcel {
    
    @Bean
    public EasyExcelConverterTool easyExcelAutoConfig(EasyExcelProp prop) {
        if(!prop.getEnhancedConverter()){
            return null;
        }
        EasyExcelConverterTool.init();
        log.debug("EasyExcelConverterTool Configured");
        return null;
    }
    
    
}
