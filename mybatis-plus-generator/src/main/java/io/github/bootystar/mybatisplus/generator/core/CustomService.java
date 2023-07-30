package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.generator.core.entity.InsertDto;
import io.github.bootystar.mybatisplus.generator.core.entity.SelectDto;
import io.github.bootystar.mybatisplus.generator.core.entity.UpdateDto;
import io.github.bootystar.mybatisplus.generator.core.entity.Vo;

import java.io.Serializable;

/**
 * service接口
 * @Author booty
 * @Date 2023/7/13 10:41
 */
public interface CustomService<T, P extends SelectDto<T> > extends IService<T> {

    T insertByDto(InsertDto<T> dto);

    Boolean updateByDto(UpdateDto<T> dto);

    IPage<Vo<T>> pageByDto(P dto);

    void exportExcel(P dto , Class<? extends Vo<T>> clazz);
}
