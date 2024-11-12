package io.github.bootystar.autoconfigure;

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
import io.github.bootystar.autoconfigure.prop.DatabindProp;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.TimeZone;


@Slf4j
@Configuration(proxyBeanMethods = false)
@EnableConfigurationProperties(DatabindProp.class)
public class Config4databind {

    @Bean
    @ConditionalOnMissingBean(value = MappingJackson2HttpMessageConverter.class)
    public MappingJackson2HttpMessageConverter mappingJackson2HttpMessageConverter(DatabindProp prop) {
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

        if (prop.getTimePackSupport()) {
            String dateTimeFormat = prop.getDateTimeFormat();
            String dateFormat = prop.getDateFormat();
            String timeFormat = prop.getTimeFormat();

            SimpleDateFormat oldFormatter = new SimpleDateFormat(dateTimeFormat);
            // 指定时区
            objectMapper.setTimeZone(TimeZone.getTimeZone("GMT+8:00"));
            // 日期类型字符串处理
            objectMapper.setDateFormat(oldFormatter);
            DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(dateTimeFormat);
            DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern(dateFormat);
            DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern(timeFormat);

            // java8日期日期处理
            JavaTimeModule javaTimeModule = new JavaTimeModule();
            javaTimeModule.addSerializer(LocalDateTime.class, new LocalDateTimeSerializer(dateTimeFormatter));
            javaTimeModule.addSerializer(LocalDate.class, new LocalDateSerializer(dateFormatter));
            javaTimeModule.addSerializer(LocalTime.class, new LocalTimeSerializer(timeFormatter));
            javaTimeModule.addDeserializer(LocalDateTime.class, new LocalDateTimeDeserializer(dateTimeFormatter));
            javaTimeModule.addDeserializer(LocalDate.class, new LocalDateDeserializer(dateFormatter));
            javaTimeModule.addDeserializer(LocalTime.class, new LocalTimeDeserializer(timeFormatter));
            objectMapper.registerModule(javaTimeModule);
        }

        if (prop.getLongToString() || prop.getDoubleToString()) {
            // long和Double转string, 避免前端精度丢失
            SimpleModule simpleModule = new SimpleModule();
            if (prop.getLongToString()) simpleModule.addSerializer(Long.class, ToStringSerializer.instance);
            if (prop.getDoubleToString()) simpleModule.addSerializer(Double.class, ToStringSerializer.instance);
            objectMapper.registerModule(simpleModule);
            converter.setObjectMapper(objectMapper);
        }
        log.debug("MappingJackson2HttpMessageConverter Configured");
        return converter;
    }

    @Bean
    public String2LocalDateTimeConverter string2LocalDateTimeConverter(DatabindProp prop) {
        if (!prop.getTimePackSupport()) return null;
        log.debug("String2LocalDateTimeConverter Configured");
        return new String2LocalDateTimeConverter(prop.getDateTimeFormat());
    }

    @Bean
    public String2LocalDateConverter string2LocalDateConverter(DatabindProp prop) {
        if (!prop.getTimePackSupport()) return null;
        log.debug("String2LocalDateConverter Configured");
        return new String2LocalDateConverter(prop.getDateFormat());
    }

    @Bean
    public String2LocalTimeConverter string2LocalTimeConverter(DatabindProp prop) {
        if (!prop.getTimePackSupport()) return null;
        log.debug("String2LocalTimeConverter Configured");
        return new String2LocalTimeConverter(prop.getTimeFormat());
    }

}
