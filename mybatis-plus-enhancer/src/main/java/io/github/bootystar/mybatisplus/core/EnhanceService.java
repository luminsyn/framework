package io.github.bootystar.mybatisplus.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.List;


/**
 * @author booty
 */
public interface EnhanceService<T,V> extends IService<T> {

    <S> V insertByDTO(S s);

    <S> boolean updateByDTO(S s);

    V voById(Serializable id);

    <U> U voById(Serializable id, Class<U> clazz);

    <S> V oneByDTO(S s);

    <S,U> U oneByDTO(S s, Class<U> clazz);

    <S> List<V> listByDTO(S s);

    <S,U> List<U> listByDTO(S s, Class<U> clazz);

    <S> IPage<V> pageByDTO(S s, Long current, Long size);

    <S,U> IPage<U> pageByDTO(S s, Long current, Long size, Class<U> clazz);

    <S,U> void exportExcel(S s, OutputStream os, Class<U> clazz, String... includeFields);

    <S,U> void exportExcel(S s, OutputStream os, Class<U> clazz, Long current, Long size, String... includeFields);

    <U> void excelTemplate(OutputStream os, Class<U> clazz);

    <U> boolean importExcel(InputStream is, Class<U> clazz);

    T toEntity(Object source);

    V toVO(Object source);

}
