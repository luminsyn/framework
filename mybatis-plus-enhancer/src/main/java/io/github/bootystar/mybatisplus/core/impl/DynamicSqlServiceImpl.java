package io.github.bootystar.mybatisplus.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.param.Condition;
import io.github.bootystar.mybatisplus.core.helper.SqlHelper;
import io.github.bootystar.mybatisplus.core.helper.UnmodifiableSqlHelper;
import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * 动态查询实现
 *
 * @author bootystar
 */
public abstract class DynamicSqlServiceImpl<M extends EnhanceMapper<T, V, UnmodifiableSqlHelper<T>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s == null) {
            return getBaseMapper().listByDTO(null, page);
        }
        if (s instanceof SqlHelper) {
            SqlHelper sqlHelper = (SqlHelper) s;
            return getBaseMapper().listByDTO(sqlHelper.unmodifiable(classOfEntity()), page);
        }
        if (s instanceof UnmodifiableSqlHelper<?>) {
            UnmodifiableSqlHelper<?> unmodifiableSqlHelper = (UnmodifiableSqlHelper<?>) s;
            if (!classOfEntity().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            return getBaseMapper().listByDTO((UnmodifiableSqlHelper<T>) unmodifiableSqlHelper, page);
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
                Condition dto = new Condition(key.toString(), value);
                conditions.add(dto);
            }
            sqlHelper.addConditions(conditions);
        } else {
            sqlHelper.addConditions(s, SqlKeyword.EQ.keyword);
        }
        if (sqlHelper.getConditions() == null || sqlHelper.getConditions().isEmpty()) {
            throw new IllegalStateException(String.format("no conditions from %s for entity %s", s.getClass().getName(), classOfEntity().getName()));
        }
        return getBaseMapper().listByDTO(sqlHelper.unmodifiable(classOfEntity()), page);
    }


    @Override
    public V oneById(Serializable id) {
        if (id == null) {
            throw new IllegalArgumentException("id can't be null");
        }
        String s = MybatisPlusReflectHelper.idFieldPropertyName(classOfEntity());
        if (s == null) {
            throw new IllegalArgumentException("no id field found in entity");
        }
        Condition condition = new Condition(s, SqlKeyword.EQ.keyword, id);
        return oneByDTO(new SqlHelper().addRequiredConditions(condition).unmodifiable(classOfEntity()));
    }

}
