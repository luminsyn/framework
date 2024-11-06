package io.github.bootystar.mybatisplus.base.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;


/**
 * 自定义Mapper
 *
 * @author bootystar
 */
public interface CustomMapper<T, V> extends BaseMapper<T> {

    List<V> listByMap(@Param("map") Map<String, Object> map, IPage<V> page);

}
