package io.github.bootystar.mybatisplus.core;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.injection.Injector;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;


/**
 * mapper
 *
 * @author bootystar
 */
public interface EnhanceMapper<T, V> extends BaseMapper<T> {

    List<V> listByInjector(@Param("injector") Injector.SafetyInjector<T> injector, @Param("page") IPage<V> page);

}
