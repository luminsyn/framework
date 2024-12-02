package io.github.bootystar.mybatisplus.enhance.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhance.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.enhance.core.EnhanceService;
import io.github.bootystar.mybatisplus.enhance.builder.FieldSuffixBuilder;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.unmodifiable.ExtraFieldSqlHelper;

import java.util.List;


/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class ExtraFieldServiceImpl<M extends EnhanceMapper<T, V, ExtraFieldSqlHelper<T>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    protected FieldSuffixBuilder suffixBuilder;

    {
        suffixBuilder = initSuffixBuilder();
    }

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
            sqlHelper = new ExtraFieldSqlHelper<>(SqlHelper.of(s), classOfEntity(), suffixBuilder);
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

    protected FieldSuffixBuilder initSuffixBuilder() {
        return null;
    }

}
