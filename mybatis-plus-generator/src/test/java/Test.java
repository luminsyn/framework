import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.rules.DateType;
import io.github.bootystar.mybatisplus.generator.CrudGenerator;

import java.util.Collections;


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
        CrudGenerator generator = new CrudGenerator(url, username, password);

        generator
                .globalConfigBuilder()
                .dateType(DateType.TIME_PACK)
//                .enableSwagger()
                .outputDir(projectPath+ "/mybatis-plus-generator/src/test/java")

        ;  // 指定输出目录
        generator
                .customConfigBuilder()
//                .returnResultClass(ReturnResult.class)
//                .returnResultGenericType(true)
//                .returnResultDefaultStaticMethodName("success")
//                .pageByDto(false)
//                .exportExcel(true)
//                .dtoPackage("dto")
//                .voPackage("vo")
                .jakartaApi(false)
                .exportExtendsVo(true)
                .voExtendsEntity(true)
                .importExtendsEntity(true)
                .enableValidated(true)
//                .requestBody(true)
                .orderColumn("age",true)
                .orderColumn("name", false)
                .orderColumn("id_card", true)
                .dtoPackage("entity.dto")
                .voPackage("entity.vo")
                .listenerPackage("entity.listener")
                .requestBody(true)
                .baseUrl("/admin/apiV2")
                .enableOrigins(true)
                .fileOverride(true)
                .voResultMap(true)

//                .voResultMap(true)
        ;
        generator.strategyConfigBuilder()
                .entityBuilder()
                .enableActiveRecord()
                .idType(IdType.ASSIGN_ID)

                .enableTableFieldAnnotation()
                .logicDeleteColumnName("deleted")
                .enableFileOverride()

        ;

        generator.strategyConfigBuilder()
                .serviceBuilder()
                .enableFileOverride()
            ;

        generator.strategyConfigBuilder()
                .mapperBuilder().enableFileOverride()
        ;
        generator.strategyConfigBuilder()
                .entityBuilder()
                .enableFileOverride()
        ;



        generator
                .packageConfigBuilder()
                .parent("bootystar.test")
                .entity("entity.pojo")
                .pathInfo(Collections.singletonMap(OutputFile.xml, projectPath + "/mybatis-plus-generator/src/test/resources/xml"))
        ;
        generator.globalConfigBuilder()
//                .enableSwagger()
                .enableSpringdoc()
        ;

        generator.execute("user");
    }

}
