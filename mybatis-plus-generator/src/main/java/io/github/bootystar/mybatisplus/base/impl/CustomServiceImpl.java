package io.github.bootystar.mybatisplus.base.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.base.GenericMapper;
import io.github.bootystar.mybatisplus.base.GenericService;
import io.github.bootystar.mybatisplus.util.ReflectUtil;

import java.util.List;


/**
 * 自写mapper生成类
 *
 * @author booty
 */
public abstract class CustomServiceImpl<M extends GenericMapper<T, V>, T, V> extends ServiceImpl<M, T> implements GenericService<T, V> {

    @Override
    public <S> List<V> doSelect(S s, IPage<V> page) {
        return getBaseMapper().listByGeneric(ReflectUtil.objectToMap(s), page);
    }

}
