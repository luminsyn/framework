import com.baomidou.mybatisplus.generator.config.rules.DateType;
import io.github.bootystar.mybatisplus.generator.BaseGenerator;
import io.github.bootystar.mybatisplus.generator.CrudGenerator;


/**
 * @Author booty
 * @Date 2023/7/13 14:44
 */
public class TestXnqlWeb {

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
                .parent("io.renren.modules.civilizedcity")
                .controller("controller.cs")
                .service("service.cs")
                .serviceImpl("service.cs.impl")
                .mapper("dao.cs")
                .xml("dao.cs.xml")
                ;
        generator.globalConfigBuilder()
                .enableSwagger()
                .dateType(DateType.ONLY_DATE)
                .outputDir(System.getProperty("user.dir")+ "/mybatisplus-generator/src/test/java")
        ;


        generator.strategyConfigBuilder().mapperBuilder()
        ;

        generator.strategyConfigBuilder().entityBuilder().enableLombok()


//                .columnNaming(NamingStrategy.no_change)


        ;
        generator.customConfigBuilder().returnResultClass("io.renren.common.utils.Result");

        generator.execute("cs_album","cs_customized_service","cs_evaluation","cs_order","cs_type","cs_service_org");
    }

}
