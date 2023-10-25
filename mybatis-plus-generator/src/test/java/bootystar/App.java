package bootystar;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.env.Environment;


import java.net.InetAddress;

/**
 * @Author booty
 * @Date 2023/9/14 15:42
 */
@Slf4j
@SpringBootApplication
public class App {
    public static void main(String[] args) throws Exception {
        ConfigurableApplicationContext application = SpringApplication.run(App.class, args);
        Environment env = application.getEnvironment();
        String host= InetAddress.getLocalHost().getHostAddress();
        String port=env.getProperty("server.port");
        String context=env.getProperty("server.servlet.context-path");
        if (context==null){
            context="";
        }
        if (!context.startsWith("/")){
            context="/"+context;
        }
        if (!context.endsWith("/")){
            context=context+"/";
        }
        log.info("Application started doc at: http://"+host+":"+port+context+"swagger-ui/index.html");

//        ClassPathResource resource = new ClassPathResource("logback.xml");
//        File file = resource.getFile();
//        System.out.println(file.getAbsolutePath());
//        System.out.println(file.exists());
    }
}
