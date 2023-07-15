import io.github.bootystar.mybatisplus.generator.BaseGenerator;
import io.github.bootystar.mybatisplus.generator.CrudGenerator;


/**
 * @Author booty
 * @Date 2023/7/13 14:44
 */
public class TestXnqlWechat {

//    private static String url ="jdbc:mysql://localhost:3306/ds_wwsg_wm?useUnicode=true&characterEncoding=UTF-8";
//    private static String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
    private static String url ="jdbc:mysql://175.24.125.34:3307/ds_wwsg_wm_test?useUnicode=true&characterEncoding=UTF-8&serverTimezone=Asia/Shanghai&nullCatalogMeansCurrent=true";
//    private static String username ="root";
    private static String username ="luomingxin";
//    private static String password ="root";
    private static String password ="Xnql.luomingxin@123";
    public static void main(String[] args) {
        BaseGenerator generator = new CrudGenerator(url, username, password);

        generator.packageConfigBuilder()
                .parent("com.xnql.cc")
                .controller("controller.api.cs")
                .service("service.cs")
                .serviceImpl("service.cs.impl")
                .mapper("mapper.cs")
                .xml("mapper.cs.xml")
                ;
        generator.globalConfigBuilder()
                .enableSwagger()

                .outputDir(System.getProperty("user.dir")+ "/mybatisplus-generator/src/test/java");  // 指定输出目录


        generator.strategyConfigBuilder().mapperBuilder().enableBaseResultMap();

        generator.strategyConfigBuilder().entityBuilder().enableLombok().enableTableFieldAnnotation()
//                .columnNaming(NamingStrategy.no_change)


        ;
        generator.customConfigBuilder().returnResultClass("com.xnql.cc.model.out.Result");

        generator.execute("cs_album","cs_customized_service","cs_evaluation","cs_order","cs_type","cs_service_org");
    }

}
