package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.github.bootystar.mybatisplus.generator.core.entity.SelectDto;
import io.github.bootystar.mybatisplus.generator.core.entity.Vo;
import org.apache.ibatis.annotations.Param;


/**
 * mapper接口
 * @Author booty
 * @Date 2023/7/13 10:41
 */
public interface CustomMapper<T ,P extends SelectDto<T>> extends BaseMapper<T> {


    IPage<Vo<T>> pageByDto(@Param("dto") P dto, Page<Vo<T>> page);

}
