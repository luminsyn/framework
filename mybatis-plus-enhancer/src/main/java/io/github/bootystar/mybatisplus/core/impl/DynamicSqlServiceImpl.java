package io.github.bootystar.mybatisplus.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.core.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.core.helper.SqlHelper;
import io.github.bootystar.mybatisplus.core.helper.SqlHelper4Dynamic;
import io.github.bootystar.mybatisplus.core.param.base.ISqlCondition;
import io.github.bootystar.mybatisplus.core.param.normal.ConditionN;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;

import java.io.Serializable;
import java.util.List;

/**
 * 动态查询实现
 *
 * @author bootystar
 */
public abstract class DynamicSqlServiceImpl<M extends EnhanceMapper<T, V, SqlHelper4Dynamic<T>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        SqlHelper4Dynamic<T> sqlHelper;
        if (s instanceof SqlHelper4Dynamic<?>) {
            SqlHelper4Dynamic<?> unmodifiableSqlHelper = (SqlHelper4Dynamic<?>) s;
            if (!classOfEntity().equals(unmodifiableSqlHelper.getEntityClass())) {
                throw new UnsupportedOperationException("not support this type of sqlHelper: " + unmodifiableSqlHelper.getEntityClass().getName());
            }
            sqlHelper = (SqlHelper4Dynamic<T>) s;
        } else if (s instanceof ISqlCondition) {
            SqlHelper helper = new SqlHelper();
            helper.addConditions((ISqlCondition) s);
            sqlHelper = SqlHelper4Dynamic.of(helper, classOfEntity());
        }else{
            sqlHelper = SqlHelper4Dynamic.of(SqlHelper.of(s, SqlKeyword.EQ.keyword), classOfEntity());
        }
        return getBaseMapper().listByDTO(sqlHelper, page);
    }

}
