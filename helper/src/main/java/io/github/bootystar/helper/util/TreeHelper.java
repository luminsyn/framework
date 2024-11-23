package io.github.bootystar.helper.util;

import lombok.Data;
import org.apache.poi.ss.formula.functions.T;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author bootystar
 */
public abstract class TreeHelper {


    /**
     * 构建树状关联关系
     *
     * @param idGetter       id getter
     * @param parentIdGetter 父id getter
     * @param childrenSetter 子元素列表setter
     * @param sourceList     元素列表
     * @return {@link List }<{@link T }> 元素列表
     * @author bootystar
     */
    public static <T> List<T> buildRelation(Function<T, ?> idGetter, Function<T, ?> parentIdGetter, BiConsumer<T, ? super List<T>> childrenSetter, Collection<T> sourceList) {
        if (idGetter == null || parentIdGetter == null || childrenSetter == null || sourceList == null || sourceList.isEmpty()) {
            return Collections.emptyList();
        }
        ArrayList<T> ts = new ArrayList<>(sourceList);
        Iterator<T> it = ts.iterator();
        while (it.hasNext()) {
            T t = it.next();
            childrenSetter.accept(t, findChildren(idGetter, parentIdGetter, ts, t));
            it.remove();
        }
        return new ArrayList<>(sourceList);
    }


    /**
     * 构建根目录树
     *
     * @param sourceList     源数据列表
     * @param idGetter       id getter
     * @param parentIdGetter 父id getter
     * @param childrenSetter 子元素列表setter
     * @return {@link List }<{@link T }> 根元素列表
     * @author bootystar
     */
    public static <T> List<T> buildRootTree(Function<T, ?> idGetter, Function<T, ?> parentIdGetter, BiConsumer<T, ? super List<T>> childrenSetter, Collection<T> sourceList) {
        List<T> ts = buildRelation(idGetter, parentIdGetter, childrenSetter, sourceList);
        if (ts.isEmpty()) {
            return Collections.emptyList();
        }
        ArrayList<T> roots = new ArrayList<>();
        for (T t : ts) {
            if (parentIdGetter.apply(t) == null) {
                roots.add(t);
            }
        }
        return roots;
    }

    /**
     * 构建当前节点对应树
     *
     * @param idGetter       id getter
     * @param parentIdGetter 父id getter
     * @param childrenSetter 子元素列表setter
     * @param sourceList     源列表
     * @param currentNode    当前节点
     * @return {@link T }    当前节点的树
     * @author bootystar
     */
    public static <T> T buildCurrentNodeTree(Function<T, ?> idGetter, Function<T, ?> parentIdGetter, BiConsumer<T, ? super List<T>> childrenSetter, Collection<T> sourceList, T currentNode) {
        buildRelation(idGetter, parentIdGetter, childrenSetter, sourceList);
        return currentNode;
    }


    /**
     * 检索当前元素的直接子元素
     *
     * @param idGetter       id getter
     * @param parentIdGetter 父id getter
     * @param sourceList     源列表
     * @param currentNode    当前元素
     * @return {@link List }<{@link T }> 直接子元素列表
     * @author bootystar
     */
    public static <T> List<T> findChildren(Function<T, ?> idGetter, Function<T, ?> parentIdGetter, Collection<T> sourceList, T currentNode) {
        if (idGetter == null || parentIdGetter == null || sourceList == null || sourceList.isEmpty() || currentNode == null) {
            return null;
        }
        Object id = idGetter.apply(currentNode);
        return sourceList.stream()
                .filter(c -> Objects.equals(id, parentIdGetter.apply(c)))
                .collect(Collectors.toCollection(ArrayList::new));
    }


    /**
     * 检索当前元素的所有子元素
     *
     * @param idGetter       id getter
     * @param parentIdGetter 父id getter
     * @param sourceList     源列表
     * @param currentNode    当前元素
     * @return {@link List }<{@link T }> 所有子元素列表
     * @author bootystar
     */
    public static <T> List<T> findAllChildren(Function<T, ?> idGetter, Function<T, ?> parentIdGetter, Collection<T> sourceList, T currentNode) {
        if (idGetter == null || parentIdGetter == null || sourceList == null || sourceList.isEmpty() || currentNode == null) {
            return null;
        }
        ArrayList<T> ts = new ArrayList<>(sourceList);
        List<T> children = findChildren(idGetter, parentIdGetter, ts, currentNode);
        if (children == null || children.isEmpty()) {
            return null;
        }
        ArrayList<T> result = new ArrayList<>(children);
        for (T child : children) {
            List<T> allChildren = findAllChildren(idGetter, parentIdGetter, ts, child);
            if (allChildren == null || allChildren.isEmpty()) {
                continue;
            }
            ts.removeAll(allChildren);
            result.addAll(allChildren);
        }
        return result;
    }

    public static void main(String[] args) {
        ArrayList<ABC> list = new ArrayList<>();
        for (int i = 0; i < 3; i++) {
            ABC abc = new ABC();
            abc.setId(i + "");
            list.add(abc);
            for (int j = 3; j < 6; j++) {
                ABC abc1 = new ABC();
                abc1.setId(i + "" + j);
                abc1.setPid(i + "");
                list.add(abc1);
                for (int k = 6; k < 9; k++) {
                    ABC abc2 = new ABC();
                    abc2.setId(i + "" + j + k);
                    abc2.setPid(i + "" + j);
                    list.add(abc2);
                }
            }
        }

        List<ABC> ts = buildRootTree(ABC::getId, ABC::getPid, ABC::setChildren, list);
        List<ABC> allChildren = findAllChildren(ABC::getId, ABC::getPid, list, ts.get(1));
        ABC abc = buildCurrentNodeTree(ABC::getId, ABC::getPid, ABC::setChildren, list, ts.get(2));
        System.out.println();
    }

    @Data
    public static class ABC {
        private String id;
        private String pid;
        private List<ABC> children;

    }


}
