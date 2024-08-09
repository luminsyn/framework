import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.generator.ParentGenerator;
import lombok.Data;
import org.junit.jupiter.api.Test;

import java.util.List;

/**
 * @author booty
 */
public class ReflectTest {
    private static String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
    private static String username ="root";
    private static String password ="root";
    @Test
    void test() {
        ParentGenerator generator = new ParentGenerator(url, username, password);
        generator.customConfigBuilder()
                .pageMethod(PageResult::fromIPage)
                .pageMethod(PageResult::new)
        ;
        
    }
    
    @Data
    static class PageResult<T> {
        private List<T> records;
        private Long current;
        private Long pages;

        public PageResult() {
        }

        public PageResult(IPage<T> page) {
            this.records = page.getRecords();
            this.current = page.getCurrent();
            this.pages = page.getPages();
        }
        public static <T>PageResult<T>  fromIPage(IPage<T> page) {
            PageResult<T> result = new PageResult<>();
            result.records = page.getRecords();
            result.current = page.getCurrent();
            result.pages = page.getPages();
            return result;
        }
    }
    
}
