package io.github.bootystar.mybatisplus.enhance.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhance.EnhanceMapper;
import io.github.bootystar.mybatisplus.enhance.EnhanceService;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicSqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.ExtraFieldSqlHelper;

import java.util.List;


/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class ExtraFieldServiceImpl<M extends EnhanceMapper<T, V, ExtraFieldSqlHelper<T>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        ExtraFieldSqlHelper<T> sqlHelper;
        if (s instanceof ExtraFieldSqlHelper<?>) {
            ExtraFieldSqlHelper<?> unmodifiableSqlHelper = (ExtraFieldSqlHelper<?>) s;
            if (!classOfEntity().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (ExtraFieldSqlHelper<T>) s;
        } else {
            sqlHelper = new ExtraFieldSqlHelper<>(SqlHelper.of(s), classOfEntity());
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

}
