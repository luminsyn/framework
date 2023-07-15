import io.github.bootystar.mybatisplus.generator.BaseGenerator;
import io.github.bootystar.mybatisplus.generator.CrudGenerator;


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
        BaseGenerator generator = new CrudGenerator(url, username, password);

        generator.globalConfigBuilder()
                .outputDir(System.getProperty("user.dir")+ "/mybatisplus-generator/src/test/java");  // 指定输出目录

        generator.packageConfigBuilder().parent("com.test");


        generator.execute("cc_app_user");
    }

}
