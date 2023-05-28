import io.github.bootystar.util.DateUtil;
import org.junit.jupiter.api.Test;

import java.util.Date;

/**
 * @author booty
 * @date 2023/5/28 12:56
 */
public class DateTest {

    @Test
    void test1(){
        Date date = DateUtil.string2DateTime("2020-10-10 12:12:12");
        System.out.println(date);
        Date date2 = DateUtil.string2Time("12:12:12");
        System.out.println(date2);
        Date date3 = DateUtil.string2Date("2020-10-10");
        System.out.println(date3);
    }
}
