package io.github.bootystar.mybatisplus.injection;

import io.github.bootystar.mybatisplus.util.ReflectUtil;
import java.util.*;

/**
 * @author bootystar
 */
public interface Injectable {

    Map<String, String> extraMap();

    default Map<String, String> injectMap() {
        Map<String, String> fieldConvertMap = ReflectUtil.fieldConvertMap(getClass());
        Map<String, String> extraMap = extraMap();
        if (extraMap!=null && !extraMap.isEmpty()){
            Iterator<Map.Entry<String, String>> it = extraMap.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry<String, String> next = it.next();
                String fieldName = next.getKey();
                String jdbcColumn = next.getValue();
                if (jdbcColumn==null || jdbcColumn.isEmpty()) {
                    continue;
                }
                if (!jdbcColumn.contains(".")){
                    jdbcColumn= String.format("a.`%s`", jdbcColumn);
                }
                fieldConvertMap.put(fieldName, jdbcColumn);
            }
        }
        return fieldConvertMap;
    }


}
