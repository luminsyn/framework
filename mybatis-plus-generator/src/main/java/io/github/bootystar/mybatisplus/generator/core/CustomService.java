package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.io.OutputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;


/**
 * @Author booty
 * @Date 2023/8/21 9:44
 */
public interface CustomService<T,V> extends IService<T> {
    T insertByDto(Dto<T> dto);
    boolean insertBatchByDto(Collection<Dto<T>> dtoList);
    boolean updateByDto(Dto<T> dto);
    V getVoById(Serializable id);
    List<V> listByDto(Dto<T> dto);
    IPage<V> pageByDto(Dto<T> dto,Long current, Long size);
    IPage<V> pageByMap(Map<String,Object> map, Long current, Long size);
    void exportExcel(Dto<T> dto, OutputStream os, Class<?> clazz);
    void exportExcel(Dto<T> dto, OutputStream os, Class<?> clazz,Collection<String> includeFields);

}
