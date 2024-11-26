package io.github.bootystar.mybatisplus.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.core.EnhanceMapper;
import io.github.bootystar.mybatisplus.core.EnhanceService;
import io.github.bootystar.mybatisplus.util.ReflectHelper4MybatisPlus;
import org.apache.poi.ss.formula.functions.T;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.*;


/**
 * 基础实现
 *
 * @author bootystar
 */
public abstract class ExtraFieldServiceImpl<M extends EnhanceMapper<T, V, Map<?, ?>>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s == null) {
            return getBaseMapper().listByDTO(null, page);
        }
        return getBaseMapper().listByDTO(ReflectHelper4MybatisPlus.objectToMap(s), page);
    }

}
