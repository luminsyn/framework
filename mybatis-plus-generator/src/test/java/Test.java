import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.rules.DateType;
import com.baomidou.mybatisplus.generator.config.rules.NamingStrategy;
import io.github.bootystar.mybatisplus.generator.BaseGenerator;
import io.github.bootystar.mybatisplus.generator.CrudGenerator;

import java.util.Collections;
import java.util.Map;


/**
 * @Author booty
 * @Date 2023/7/13 14:44
 */
public class Test {

//    private static String url ="jdbc:mysql://localhost:3306/ds_wwsg_wm?useUnicode=true&characterEncoding=UTF-8";
    private static String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
    private static String username ="root";
    private static String password ="root";
    public static void main(String[] args) {
        String projectPath = System.getProperty("user.dir");
        BaseGenerator generator = new CrudGenerator(url, username, password);

        generator
                .globalConfigBuilder()
                .enableSwagger()
                .outputDir(projectPath+ "/mybatis-plus-generator/src/test/java")

        ;  // 指定输出目录
        generator
                .customConfigBuilder()
                .dtoPackage("dto")
                .voPackage("vo")
        ;
        generator.strategyConfigBuilder()
                .entityBuilder()
//                .disableSerialVersionUID()
//                .columnNaming(NamingStrategy.no_change)
        ;

        generator.strategyConfigBuilder()
                .controllerBuilder()
                .enableFileOverride()

                ;

        generator
                .packageConfigBuilder()
                .parent("bootystar.test")
//                .pathInfo(Collections.singletonMap(OutputFile.xml, projectPath + "/mybatis-plus-generator/src/test/resources/xml"))
        ;

        generator
                .customConfigBuilder()
                .voResultMap(true)
                .orderColumn("age",true)
                .orderColumn("name", false)
                .orderColumn("id_card", true)

        ;

        generator.execute("cc_app_user");
    }

}
