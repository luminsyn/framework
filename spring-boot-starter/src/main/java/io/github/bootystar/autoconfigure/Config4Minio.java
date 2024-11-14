//package io.github.bootystar.autoconfigure;
//
//import io.github.bootystar.autoconfigure.prop.MinioProp;
//import io.github.bootystar.helper.minio.MinioHelper;
//import io.minio.MinioClient;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
//import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
//import org.springframework.boot.context.properties.EnableConfigurationProperties;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//
///**
// * @author bootystar
// */
//@Slf4j
//@Configuration
//@EnableConfigurationProperties(MinioProp.class)
//@ConditionalOnClass({MinioClient.class, MinioHelper.class})
//public class Config4Minio {
//
//    @Bean
//    @ConditionalOnMissingBean(MinioHelper.class)
//    public MinioHelper minioEnhancedClient(MinioProp minioProp) {
//        Boolean enable = minioProp.getEnable();
//        if (!enable) {
//            return null;
//        }
//        MinioClient build = MinioClient.builder().endpoint(minioProp.getEndpoint())
//                .credentials(minioProp.getAccessKey(), minioProp.getSecretKey())
//                .build();
//        log.debug("MinioHelper Configured");
//        return new MinioHelper(build, minioProp.getBucketName());
//    }
//
//}
