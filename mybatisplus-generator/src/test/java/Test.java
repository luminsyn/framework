import com.baomidou.mybatisplus.generator.config.StrategyConfig;
import io.github.bootystar.mybatisplus.generator.BaseGenerator;
import io.github.bootystar.mybatisplus.generator.CrudGenerator;


/**
 * @Author booty
 * @Date 2023/7/13 14:44
 */
public class Test {

//    private static String url ="jdbc:mysql://localhost:3306/ds_wwsg_wm?useUnicode=true&characterEncoding=UTF-8";
    private static String url ="jdbc:mysql://192.168.1.20:3306/xiaotong_sa_custom?useUnicode=true&characterEncoding=UTF-8";
    private static String username ="root";
    private static String password ="root";
    public static void main(String[] args) {
        BaseGenerator generator = new CrudGenerator(url, username, password);
        StrategyConfig.Builder builder = generator.strategyConfigBuilder();
//        builder.controllerBuilder().superClass(CustomController.class);
//        builder.serviceBuilder().superServiceClass(CustomService.class);
//        builder.serviceBuilder().superServiceImplClass(CustomServiceImp.class);
//        builder.mapperBuilder().superClass(CustomMapper.class);

        String projectPath = System.getProperty("user.dir");
        generator.globalConfigBuilder()
                .outputDir( projectPath+ "/mybatisplus-generator/src/main/java");  // 指定输出目录

        generator.packageConfigBuilder().parent("com.test");

        generator.execute("community_asset");
    }

}
