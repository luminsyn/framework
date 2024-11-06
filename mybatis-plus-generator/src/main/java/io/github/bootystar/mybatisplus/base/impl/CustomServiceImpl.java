package io.github.bootystar.mybatisplus.base.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.base.IBaseService;
import io.github.bootystar.mybatisplus.base.mapper.CustomMapper;
import io.github.bootystar.mybatisplus.util.ReflectUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.*;


/**
 * 自定义Service实现类
 *
 * @author booty
 */
@Slf4j
public abstract class CustomServiceImpl<M extends CustomMapper<T, V>, T, V> extends ServiceImpl<M, T> implements IBaseService<T, V> {

    @Override
    public <S> List<V> doSelect(S s, IPage<V> page) {
        return getBaseMapper().listByMap(ReflectUtil.objectToMap(s), page);
    }

}
