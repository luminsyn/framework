package io.github.bootystar.mybatisplus.enhance.helper;

import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicSqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.ExtraFieldSqlHelper;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import io.github.bootystar.mybatisplus.enhance.query.general.SortG;
import io.github.bootystar.mybatisplus.enhance.query.general.TreeG;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.SneakyThrows;

import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 动态sql助手
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper extends TreeG {

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     *
     * @param entity   实体
     * @param operator 操作符号
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addRequiredConditions(Object entity, String operator) {
        return addRequiredConditions(conditionsFromEntity(entity, operator));
    }

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     * 前置条件新>前置条件旧>一般条件
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addRequiredConditions(ISqlCondition... conditions) {
        return addRequiredConditions(Arrays.asList(conditions));
    }

    /**
     * 添加前置条件
     * 前置条件优先于现有条件, 必定生效
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addRequiredConditions(Collection<? extends ISqlCondition> conditions) {
        TreeG oldTree = this.getChild();
        TreeG newTree = new TreeG();
        this.setChild(newTree);
        newTree.setChild(oldTree);
        newTree.setConditions(this.getConditions());
        this.setConditions(conditions.stream().map(ConditionG::of).collect(Collectors.toCollection(LinkedHashSet::new)));
        return this;
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param entity   实体
     * @param operator 操作符 {@link SqlKeyword}
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addConditions(Object entity, String operator) {
        return addConditions(conditionsFromEntity(entity, operator));
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addConditions(ISqlCondition... conditions) {
        return addConditions(Arrays.asList(conditions));
    }

    /**
     * 添加一般条件
     * 和现有条件同等优先级
     *
     * @param conditions 条件
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addConditions(Collection<? extends ISqlCondition> conditions) {
        if (conditions == null || conditions.isEmpty()) {
            return this;
        }
        if (this.conditions == null) {
            this.conditions = new LinkedHashSet<>();
        }
        this.conditions.addAll(conditions.stream().map(ConditionG::of).collect(Collectors.toList()));
        return this;
    }

    /**
     * 添加排序
     *
     * @param sorts 排序
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addSorts(ISqlSort... sorts) {
        if (sorts == null || sorts.length == 0) {
            return this;
        }
        return addSorts(Arrays.asList(sorts));
    }

    /**
     * 添加排序
     *
     * @param sorts 排序
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public SqlHelper addSorts(Collection<ISqlSort> sorts) {
        if (sorts == null || sorts.isEmpty()) {
            return this;
        }
        if (this.sorts == null) {
            this.sorts = new LinkedHashSet<>();
        }
        this.sorts.addAll(sorts.stream().map(SortG::of).collect(Collectors.toList()));
        return this;
    }


    /**
     * 根据实体类生成条件
     *
     * @param entity   实体类
     * @param operator SQL操作符号 参考{@link SqlKeyword }
     * @return {@link List }<{@link ConditionG }>
     * @author bootystar
     */
    @SneakyThrows
    public static List<ConditionG> conditionsFromEntity(Object entity, String operator) {
        if (entity == null) {
            throw new IllegalStateException("entity is null");
        }
        Map<String, Field> fieldMap = MybatisPlusReflectHelper.fieldMap(entity.getClass());
        List<ConditionG> conditions = new ArrayList<>();
        for (Field field : fieldMap.values()) {
            Object value = field.get(entity);
            if (value == null) continue;
            ConditionG condition = new ConditionG();
            condition.setField(field.getName());
            condition.setOperator(operator);
            condition.setValue(value);
            conditions.add(condition);
        }
        return conditions;
    }

    /**
     * 根据指定对象字段映射等于条件
     * @param s s
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public static SqlHelper of(Object s){
        return of(s, SqlKeyword.EQ.keyword);
    }

    /**
     * 根据指定对象字段及操作符映射条件
     *
     * @param s        指定对象
     * @param operator SQL操作符号 参考{@link SqlKeyword }
     * @return {@link SqlHelper }
     * @author bootystar
     */
    public static SqlHelper of(Object s, String operator) {
        if (s == null) {
            return new SqlHelper();
        }
        if (s instanceof ISqlTree) {
            return ofSqlTree((ISqlTree) s);
        }
        SqlHelper sqlHelper = new SqlHelper();
        if (s instanceof ISqlCondition) {
            sqlHelper.addConditions((ISqlCondition) s);
        }
        if (s instanceof ISqlSort) {
            sqlHelper.addSorts((ISqlSort) s);
        }
        if (s instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) s;
            Iterator<? extends Map.Entry<?, ?>> iterator = map.entrySet().iterator();
            List<ConditionG> conditions = new ArrayList<>();
            while (iterator.hasNext()) {
                Map.Entry<?, ?> next = iterator.next();
                Object key = next.getKey();
                Object value = next.getValue();
                ConditionG condition = new ConditionG(key.toString(), value);
                conditions.add(condition);
            }
            sqlHelper.addConditions(conditions);
        } else {
            sqlHelper.addConditions(s, operator);
        }
        if (sqlHelper.getConditions() == null || sqlHelper.getConditions().isEmpty()) {
            throw new IllegalStateException(String.format("no conditions from %s", s));
        }
        return sqlHelper;
    }

    public static SqlHelper ofSqlTree(ISqlTree tree) {
        if (tree == null) {
            return new SqlHelper();
        }
        if (tree instanceof SqlHelper) {
            return (SqlHelper) tree;
        }
        SqlHelper sqlHelper = new SqlHelper();
        Collection<? extends ISqlCondition> conditions1 = tree.getConditions();
        if (conditions1 != null) {
            sqlHelper.addConditions(conditions1.stream().map(ConditionG::of).collect(Collectors.toList()));
        }
        Collection<? extends ISqlSort> sorts1 = tree.getSorts();
        if (sorts1 != null) {
            sqlHelper.addSorts(sorts1.stream().map(SortG::of).collect(Collectors.toList()));
        }
        TreeG thisChild = sqlHelper.getChild();
        ISqlTree thatTree = tree.getChild();
        // todo SqlTree的合并
        while (thatTree != null) {
            Collection<? extends ISqlCondition> conditions2 = thatTree.getConditions();

            if (conditions2 != null) {

                if (thisChild.getConditions() == null) {
                    thisChild.setConditions(new LinkedHashSet<>());
                }
                thisChild.getConditions().addAll(conditions2.stream().map(ConditionG::of).collect(Collectors.toList()));
            }
            thatTree = thatTree.getChild();

        }

        return sqlHelper;
    }

    public <T> DynamicSqlHelper<T> dynamicHelper(Class<T> clazz) {
        return new DynamicSqlHelper<>(this, clazz);
    }

    public <T> ExtraFieldSqlHelper<T> extraFieldHelper(Class<T> clazz) {
        return new ExtraFieldSqlHelper<>(this, clazz);
    }

}
