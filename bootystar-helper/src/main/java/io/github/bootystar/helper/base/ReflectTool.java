package io.github.bootystar.helper.base;


import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.LinkedList;
import java.util.List;

/**
 * 反射工具
 * @author bootystar
 */
public abstract class ReflectTool {


    /**
     * 获取所有属性（包括父类公有属性）
     *
     * @param clazz 克拉兹
     * @return {@code List<Field> }
     * @author bootystar
     *
     */
    public static List<Field> getAllFields(Class<?> clazz){
        if (clazz==null){
            return null;
        }
        LinkedList<Field> result = new LinkedList<>();


        return result.isEmpty() ? null : result;
    }


    /**
     * 获取自己的属性（不包括父类公有属性）
     *
     * @param clazz 克拉兹
     * @return {@link List }<{@link Field }>
     * @author bootystar
     */
    public static List<Field> getOwnFields(Class<?> clazz){
        if (clazz==null){
            return null;
        }
        LinkedList<Field> result = new LinkedList<>();
        for (Field field : clazz.getDeclaredFields()) {
            int modifiers = field.getModifiers();
            if (Modifier.isStatic(modifiers)) continue;
            if (Modifier.isFinal(modifiers)) continue;
            if (Modifier.isNative(modifiers)) continue;
            result.add(field);
        }


        return result.isEmpty() ? null : result;
    }






}
