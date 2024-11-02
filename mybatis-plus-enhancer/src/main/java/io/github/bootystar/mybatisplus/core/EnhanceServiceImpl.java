package io.github.bootystar.mybatisplus.core;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import java.util.Collections;
import java.util.List;


/**
 * service impl
 *
 * @author bootystar
 */
public abstract class EnhanceServiceImpl<M extends EnhanceMapper<T, V>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    public <S> List<V> doSelect(S s, IPage<V> page) {
        return Collections.emptyList();
    }



}
