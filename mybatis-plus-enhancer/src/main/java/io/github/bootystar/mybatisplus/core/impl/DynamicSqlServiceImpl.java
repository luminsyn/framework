package io.github.bootystar.mybatisplus.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.logic.dynamic.core.Condition;
import io.github.bootystar.mybatisplus.logic.dynamic.core.SqlHelper;
import io.github.bootystar.mybatisplus.logic.dynamic.core.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.logic.dynamic.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.util.ReflectHelper4MybatisPlus;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * 动态查询实现
 *
 * @author bootystar
 */
public abstract class DynamicSqlServiceImpl<M extends EnhanceMapper<T, V>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    public List<String> queryFields() {
        return new ArrayList<>(ReflectHelper4MybatisPlus.dynamicFieldsMap(entityClass()).keySet());
    }

    @Override
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s == null) {
            return getBaseMapper().listByDTO(null, page);
        }
        if (s instanceof SqlHelper) {
            SqlHelper sqlHelper = (SqlHelper) s;
            return getBaseMapper().listByDTO(sqlHelper.unmodifiable(entityClass()), page);
        }
        if (s instanceof UnmodifiableSqlHelper<?>) {
            UnmodifiableSqlHelper<?> unmodifiableSqlHelper = (UnmodifiableSqlHelper<?>) s;
            if (entityClass().equals(unmodifiableSqlHelper.getEntityClass())) {
                return getBaseMapper().listByDTO(unmodifiableSqlHelper, page);
            }
            throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
        }
        SqlHelper sqlHelper = new SqlHelper();
        if (s instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) s;
            Iterator<? extends Map.Entry<?, ?>> iterator = map.entrySet().iterator();
            List<Condition> conditions = new ArrayList<>();
            while (iterator.hasNext()) {
                Map.Entry<?, ?> next = iterator.next();
                Object key = next.getKey();
                Object value = next.getValue();
                Condition dto = new Condition(SqlKeyword.AND.keyword, key.toString(), SqlKeyword.EQ.keyword, value);
                conditions.add(dto);
            }
            sqlHelper.addConditions(conditions);
        } else {
            sqlHelper.addConditions(s, SqlKeyword.EQ.keyword);
        }
        if (sqlHelper.getConditions() == null || sqlHelper.getConditions().isEmpty()) {
            throw new IllegalStateException(String.format("no conditions from %s for entity %s", s.getClass().getName(), entityClass().getName()));
        }
        return getBaseMapper().listByDTO(sqlHelper.unmodifiable(entityClass()), page);
    }

}
