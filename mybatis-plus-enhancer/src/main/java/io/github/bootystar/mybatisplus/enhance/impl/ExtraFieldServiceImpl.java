package io.github.bootystar.mybatisplus.enhance.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhance.EnhanceMapper;
import io.github.bootystar.mybatisplus.enhance.EnhanceService;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhance.helper.SqlHelper4ExtraField;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;

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
        } else {
            sqlHelper = SqlHelper.of(s).extraFieldHelper(classOfEntity());
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

}
