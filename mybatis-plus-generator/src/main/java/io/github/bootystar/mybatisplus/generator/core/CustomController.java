package io.github.bootystar.mybatisplus.generator.core;

import io.github.bootystar.mybatisplus.generator.core.entity.SelectDto;
import io.github.bootystar.mybatisplus.generator.core.entity.Vo;

/**
 * 前端控制器
 * @Author booty
 * @Date 2023/7/13 10:41
 */
public class CustomController<S extends CustomService<T, P>, T, E, P extends SelectDto<T> ,V extends Vo<T>> {

    protected S baseService;



}
