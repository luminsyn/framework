package io.github.bootystar.helper.util;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author bootystar
 */
public abstract class TreeHelper {


    /**
     * 构建树
     *
     * @param sourceList     源数据列表
     * @param idGetter       id getter
     * @param parentIdGetter 父id getter
     * @param childrenSetter 子列表setter
     * @return {@link List }<{@link T }>
     * @author bootystar
     */
    public static <T> List<T> buildTree(Function<T, ?> idGetter, Function<T, ?> parentIdGetter, BiConsumer<T, ? super List<T>> childrenSetter, Collection<T> sourceList) {
        if (idGetter == null || parentIdGetter == null || childrenSetter == null || sourceList == null || sourceList.isEmpty()) {
            return Collections.emptyList();
        }
        ArrayList<T> ts = new ArrayList<>(sourceList);
        Iterator<T> it = ts.iterator();
        ArrayList<T> root = new ArrayList<>();
        while (it.hasNext()) {
            T t = it.next();
            Object parentId = parentIdGetter.apply(t);
            if (parentId == null) {
                root.add(t);
            }
            childrenSetter.accept(t, findChildren(idGetter, parentIdGetter, ts, t));
            it.remove();
        }
        return root;
    }


    public static <T> List<T> findChildren(Function<T, ?> idGetter, Function<T, ?> parentIdGetter, Collection<T> sourceList, T t) {
        if (idGetter == null || parentIdGetter == null || sourceList == null || sourceList.isEmpty() || t == null) {
            return null;
        }
        Object id = idGetter.apply(t);
        return sourceList.stream()
                .filter(c -> Objects.equals(id, parentIdGetter.apply(c)))
                .collect(Collectors.toList());
    }


}
