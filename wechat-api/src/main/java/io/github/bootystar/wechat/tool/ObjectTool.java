package io.github.bootystar.wechat.tool;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.Map;

/**
 * 对象工具
 * @author bootystar
 *
 */
public class ObjectTool {



    public static boolean isEmpty(Object obj){
        if (null == obj) {
            return true;
        }
        if (obj instanceof CharSequence) {
            CharSequence charSequence = (CharSequence) obj;
            return charSequence.length() == 0;
        } else if (obj instanceof Map) {
            Map<?,?> map = (Map<?,?>) obj;
            return map.isEmpty();
        } else if (obj instanceof Iterable) {
            Iterable<?> iterable = (Iterable<?>) obj;
            return iterable.iterator().hasNext();
        } else if (obj instanceof Iterator) {
            Iterator<?> iterator = (Iterator<?>) obj;
            return iterator.hasNext();
        } else if (obj.getClass().isArray()) {
            return 0 == Array.getLength(obj);
        }
        return false;
    }

    public static boolean isNotEmpty(Object obj){
        return !isEmpty(obj);
    }


}
