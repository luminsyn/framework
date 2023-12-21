import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.util.*;

/**
 * @author booty
 * @since 2023/11/9
 */
public class Test3 {

    @Test
    void test1() {
        List<List<String>> data =new LinkedList<>();

        LinkedList<String> header = new LinkedList<>();

        header.add("姓名");
        header.add("年龄");
        header.add("性别");
        header.add("哈哈");

        for (String string : header) {
            LinkedList<String> list = new LinkedList<>();
            list.add(string);
            data.add(list);
        }

        List<List<String>> data2 =new LinkedList<>();
        for (int i = 0; i < data.size(); i++) {
            LinkedList<String> list = new LinkedList<>();
            data2.add(list);
            list.add("数sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss据"+i);

        }

        EasyExcel.write(new File("C:\\Users\\PC\\Desktop\\test.xlsx"))
                .registerWriteHandler(new LongestMatchColumnWidthStyleStrategy())
                .head(data)
                .sheet("sheet1")
                .doWrite(data2);
    }


}
