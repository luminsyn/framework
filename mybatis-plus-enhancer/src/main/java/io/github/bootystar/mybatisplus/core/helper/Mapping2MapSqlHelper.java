package io.github.bootystar.mybatisplus.core.helper;

import io.github.bootystar.mybatisplus.core.MappingException;
import io.github.bootystar.mybatisplus.core.param.ConditionTree;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import lombok.Getter;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author bootystar
 */
@Getter
public class Mapping2MapSqlHelper<T> extends ConditionTree {
    /**
     * 实体类
     */
    private final Class<T> entityClass;
    /**
     * 额外参数
     */
    private LinkedHashMap<String, Object> params;

    public Mapping2MapSqlHelper(Object obj, Class<T> entityClass) {
        if (entityClass == null || obj == null) {
            throw new MappingException("param or entityClass class can not be null, please check your configuration");
        }
        Map<String, String> map = MybatisPlusReflectHelper.field2JdbcColumnMap(entityClass);
        if (map.isEmpty()) {
            throw new MappingException("entityClass %s has no field to convert, please check your configuration", entityClass.getName());
        }
        this.entityClass = entityClass;
    }


}
