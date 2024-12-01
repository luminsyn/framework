package io.github.bootystar.mybatisplus.enhance.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhance.EnhanceMapper;
import io.github.bootystar.mybatisplus.enhance.EnhanceService;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.DynamicSqlHelper;

import java.util.List;

/**
 * 动态查询实现
 *
 * @author bootystar
 */
public abstract class DynamicSqlServiceImpl<M extends EnhanceMapper<T, V, DynamicSqlHelper<T>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        DynamicSqlHelper<T> sqlHelper;
        if (s instanceof DynamicSqlHelper<?>) {
            DynamicSqlHelper<?> unmodifiableSqlHelper = (DynamicSqlHelper<?>) s;
            if (!classOfEntity().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (DynamicSqlHelper<T>) s;
        } else {
            sqlHelper = SqlHelper.of(s).dynamicHelper(classOfEntity());
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

}
