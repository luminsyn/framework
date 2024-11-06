package io.github.bootystar.mybatisplus.base.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.logic.injection.dto.InjectionDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;


/**
 * mapper
 *
 * @author bootystar
 */
public interface InjectionMapper<T, V> extends BaseMapper<T> {

    List<V> listByInjection(@Param("injection") InjectionDTO.ImmutableInjection<T> injection, @Param("page") IPage<V> page);

}
