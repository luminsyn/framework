package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;


/**
 * 自定义Mapper
 * @author booty
 * @since 2023/8/20 21:36
 */
public interface CustomMapper<T,V>  extends BaseMapper<T> {
    List<V> listByDto(@Param("dto") Map<String,Object> dto, IPage<V> page);

}
