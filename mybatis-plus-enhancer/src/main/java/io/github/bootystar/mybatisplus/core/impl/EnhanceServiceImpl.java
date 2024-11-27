package io.github.bootystar.mybatisplus.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;

import java.io.Serializable;
import java.util.*;


/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class EnhanceServiceImpl<M extends EnhanceMapper<T, V, Map<?, ?>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s == null) {
            return getBaseMapper().listByDTO(null, page);
        }
        return getBaseMapper().listByDTO(MybatisPlusReflectHelper.objectToMap(s), page);
    }

    @Override
    public V oneById(Serializable id) {
        String s = MybatisPlusReflectHelper.idFieldPropertyName(classOfEntity());
        HashMap<Object, Object> map = new HashMap<>();
        map.put(s, id);
        return oneByDTO(map);
    }
}
