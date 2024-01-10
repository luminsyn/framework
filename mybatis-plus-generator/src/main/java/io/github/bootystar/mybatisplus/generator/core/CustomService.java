package io.github.bootystar.mybatisplus.generator.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.*;


/**
 * @author booty
 *
 */
public interface CustomService<T,V> extends IService<T> {
    <S> V insertByDTO(S DTO);
    <S> boolean insertBatchByDTO(Collection<S> DTOList);
    <S> boolean updateByDTO(S DTO);
    V voById(Serializable id);
    <U> U voById(Serializable id, Class<U> clazz);
    <S> V oneByDTO(S DTO);
    <S,U> U oneByDTO(S DTO, Class<U> clazz);
    <S> List<V> listByDTO(S DTO);
    <S,U> List<U> listByDTO(S DTO, Class<U> clazz);
    <S> IPage<V> pageByDTO(S DTO, Long current, Long size);
    <S,U> IPage<U> pageByDTO(S DTO, Long current, Long size, Class<U> clazz);
    <S,U> void exportExcel(S DTO, OutputStream os, Class<U> clazz);
    <S,U> void exportExcel(S DTO, OutputStream os, Class<U> clazz, Collection<String> includeFields);
    <U> boolean importExcel(InputStream is, Class<U> clazz);
    <U> void exportTemplate(OutputStream os, Class<U> clazz);
    T toEntity(Object source);
    V toVO(Object source);

}
