package io.github.bootystar.mybatisplus.base.injection;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.base.injection.dto.InjectionDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;


/**
 * mapper
 *
 * @author bootystar
 */
public interface InjectionMapper<T, V> extends BaseMapper<T> {

    List<V> listByInjector(@Param("injector") InjectionDTO.SafetyInjector<T> injector, @Param("page") IPage<V> page);

}
