package io.github.bootystar.mybatisplus.base;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Param;

import java.util.List;


/**
 * 通用泛型mapper
 *
 * @author bootystar
 */
public interface GenericMapper<T, V> extends BaseMapper<T> {

    <G> List<V> listByGeneric(@Param("generic") G generic, @Param("page") IPage<V> page);

}
