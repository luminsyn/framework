package io.github.bootystar.autoconfigure;

import com.alibaba.excel.converters.DefaultConverterLoader;
import com.baomidou.mybatisplus.autoconfigure.MybatisPlusAutoConfiguration;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.BlockAttackInnerInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.OptimisticLockerInnerInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateDeserializer;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalDateTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.deser.LocalTimeDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateSerializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalDateTimeSerializer;
import com.fasterxml.jackson.datatype.jsr310.ser.LocalTimeSerializer;
import io.github.bootystar.autoconfigure.databind.converter.String2LocalDateConverter;
import io.github.bootystar.autoconfigure.databind.converter.String2LocalDateTimeConverter;
import io.github.bootystar.autoconfigure.databind.converter.String2LocalTimeConverter;
import io.github.bootystar.autoconfigure.prop.EasyExcelProp;
import io.github.bootystar.autoconfigure.prop.MinioProp;
import io.github.bootystar.helper.easyexcel.EasyExcelConverterTool;
import io.github.bootystar.helper.minio.MinioHelper;
import io.minio.MinioClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.TimeZone;
import static io.github.bootystar.autoconfigure.databind.constant.DateConst.*;

@Slf4j
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties({EasyExcelProp.class, MinioProp.class})
public class InitializingCustomConfig implements InitializingBean {

    @Bean
    @ConditionalOnMissingBean(MappingJackson2HttpMessageConverter.class)
    public MappingJackson2HttpMessageConverter mappingJackson2HttpMessageConverter() {
        MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
        ObjectMapper objectMapper = new ObjectMapper();

        // 忽略json字符串中不识别的属性
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        // 忽略无法转换的对象
        objectMapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        // PrettyPrinter 格式化输出
//        objectMapper.configure(SerializationFeature.INDENT_OUTPUT, true);
        // NULL不参与序列化
//        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);

        // 指定时区
        objectMapper.setTimeZone(TimeZone.getTimeZone("GMT+8:00"));
        // 日期类型字符串处理
        objectMapper.setDateFormat(SIMPLE_DATE_FORMATTER);

        // java8日期日期处理
        JavaTimeModule javaTimeModule = new JavaTimeModule();
        javaTimeModule.addSerializer(LocalDateTime.class, new LocalDateTimeSerializer(LOCAL_DATE_TIME_FORMATTER));
        javaTimeModule.addSerializer(LocalDate.class, new LocalDateSerializer(LOCAL_DATE_FORMATTER));
        javaTimeModule.addSerializer(LocalTime.class, new LocalTimeSerializer(LOCAL_TIME_FORMATTER));
        javaTimeModule.addDeserializer(LocalDateTime.class, new LocalDateTimeDeserializer(LOCAL_DATE_TIME_FORMATTER));
        javaTimeModule.addDeserializer(LocalDate.class, new LocalDateDeserializer(LOCAL_DATE_FORMATTER));
        javaTimeModule.addDeserializer(LocalTime.class, new LocalTimeDeserializer(LOCAL_TIME_FORMATTER));

        objectMapper.registerModule(javaTimeModule);

        // long和Double转string, 避免前端精度丢失
        SimpleModule simpleModule = new SimpleModule();
        simpleModule.addSerializer(Long.class, ToStringSerializer.instance);
        simpleModule.addSerializer(Double.class, ToStringSerializer.instance);
        objectMapper.registerModule(simpleModule);

        converter.setObjectMapper(objectMapper);
        return converter;
    }

    @Bean
    public String2LocalDateTimeConverter string2LocalDateTimeConverter() {
        return new String2LocalDateTimeConverter();
    }

    @Bean
    public String2LocalDateConverter string2LocalDateConverter() {
        return new String2LocalDateConverter();
    }

    @Bean
    public String2LocalTimeConverter string2LocalTimeConverter() {
        return new String2LocalTimeConverter();
    }


    @Bean
    @ConditionalOnMissingBean(MybatisPlusInterceptor.class)
    @ConditionalOnBean(MybatisPlusAutoConfiguration.class)
    public MybatisPlusInterceptor mybatisPlusInterceptor() {
        MybatisPlusInterceptor interceptor = new MybatisPlusInterceptor();
        // 分页插件
        PaginationInnerInterceptor paginationInnerInterceptor = new PaginationInnerInterceptor();
        interceptor.addInnerInterceptor(paginationInnerInterceptor);
        // 乐观锁插件
        OptimisticLockerInnerInterceptor optimisticLockerInnerInterceptor = new OptimisticLockerInnerInterceptor();
        interceptor.addInnerInterceptor(optimisticLockerInnerInterceptor);
        // 防全表更新插件
        BlockAttackInnerInterceptor blockAttackInnerInterceptor = new BlockAttackInnerInterceptor();
        interceptor.addInnerInterceptor(blockAttackInnerInterceptor);
        return interceptor;
    }

    @Bean
    @ConditionalOnClass({EasyExcelConverterTool.class, DefaultConverterLoader.class})
    public EasyExcelConverterTool easyExcelAutoConfig(EasyExcelProp easyExcelProp) {
        if(!easyExcelProp.getEnhancedConverter()){
            return null;
        }
        EasyExcelConverterTool.init();
        return null;
    }

    @Bean
    @ConditionalOnMissingBean(MinioHelper.class)
    @ConditionalOnClass(MinioClient.class)
    public MinioHelper minioEnhancedClient(MinioProp minioProp ) {
        Boolean enable = minioProp.getEnable();
        if(!enable){
            return null;
        }
        MinioClient build = MinioClient.builder().endpoint(minioProp.getEndpoint())
                .credentials(minioProp.getAccessKey(), minioProp.getSecretKey())
                .build();
        return new MinioHelper(build, minioProp.getBucketName());
    }

    @Override
    public void afterPropertiesSet(){
        log.debug("booty-spring-starter initialize completed");
    }
}
