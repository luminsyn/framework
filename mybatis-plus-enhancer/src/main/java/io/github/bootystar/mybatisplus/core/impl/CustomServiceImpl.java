package io.github.bootystar.mybatisplus.core.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.core.GenericMapper;
import io.github.bootystar.mybatisplus.core.GenericService;
import io.github.bootystar.mybatisplus.util.ReflectHelper4MybatisPlus;

import java.util.List;


/**
 * 自写mapper生成类
 *
 * @author booty
 */
public abstract class CustomServiceImpl<M extends GenericMapper<T, V>, T, V> extends ServiceImpl<M, T> implements GenericService<T, V> {

    @Override
    public <S> List<V> doSelect(S s, IPage<V> page) {
        return getBaseMapper().listByGeneric(ReflectHelper4MybatisPlus.objectToMap(s), page);
    }

}
