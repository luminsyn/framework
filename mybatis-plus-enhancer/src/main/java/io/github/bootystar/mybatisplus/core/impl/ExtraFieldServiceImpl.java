package io.github.bootystar.mybatisplus.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.core.helper.SqlHelper;
import io.github.bootystar.mybatisplus.core.helper.SqlHelper4ExtraField;
import io.github.bootystar.mybatisplus.core.param.base.ISqlCondition;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;


/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class ExtraFieldServiceImpl<M extends EnhanceMapper<T, V, SqlHelper4ExtraField<T>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        SqlHelper4ExtraField<T> sqlHelper;
        if (s instanceof SqlHelper4ExtraField<?>) {
            SqlHelper4ExtraField<?> unmodifiableSqlHelper = (SqlHelper4ExtraField<?>) s;
            if (!classOfEntity().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (SqlHelper4ExtraField<T>) s;
        } else if (s instanceof ISqlCondition) {
            SqlHelper helper = new SqlHelper();
            helper.addConditions((ISqlCondition) s);
            sqlHelper = SqlHelper4ExtraField.of(helper, classOfEntity());
        } else {
            sqlHelper = SqlHelper4ExtraField.of(SqlHelper.of(s, SqlKeyword.EQ.keyword), classOfEntity());
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

    @Override
    public V oneById(Serializable id) {
        if (id == null) throw new IllegalArgumentException("id can't be null");
        String s = MybatisPlusReflectHelper.idFieldPropertyName(classOfEntity());
        if (s == null) throw new IllegalArgumentException("no id field found in entity");
        HashMap<Object, Object> map = new HashMap<>();
        map.put(s, id);
        return oneByDTO(map);
    }

}
