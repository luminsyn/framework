package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.ibatis.annotations.Param;

import java.util.Map;


/**
 * 自定义Mapper
 * @Author booty
 * @Date 2023/8/20 21:36
 */
public interface CustomMapper<T,V>  extends BaseMapper<T> {
    IPage<V> pageByDto(@Param("dto") Map<String,Object> map, Page<V> page);

}
