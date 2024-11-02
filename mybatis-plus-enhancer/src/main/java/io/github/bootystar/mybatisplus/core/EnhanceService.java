package io.github.bootystar.mybatisplus.core;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.write.builder.ExcelWriterBuilder;
import com.alibaba.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.injection.Condition;
import io.github.bootystar.mybatisplus.injection.Injector;
import io.github.bootystar.mybatisplus.util.ReflectUtil;
import org.apache.ibatis.exceptions.TooManyResultsException;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;


/**
 * service
 *
 * @author bootystar
 */
public interface EnhanceService<T, V> extends IService<T> {

    default Collection<String> injectableFields() {
        return ReflectUtil.injectableFieldsMap(entityClass()).keySet();
    }

    @SuppressWarnings("unchecked")
    default Class<T> entityClass() {
        return (Class<T>) ReflectUtil.resolveTypeArguments(getClass(), EnhanceService.class)[0];
    }

    @SuppressWarnings("unchecked")
    default Class<V> voClass() {
        return (Class<V>) ReflectUtil.resolveTypeArguments(getClass(), EnhanceService.class)[1];
    }

    default T toEntity(Object source) {
        return toTarget(source, entityClass());
    }

    default V toVO(Object source) {
        return toTarget(source, voClass());
    }

    default <U> U toTarget(Object source, Class<U> clazz) {
        return ReflectUtil.copyProperties(source, ReflectUtil.newInstance(clazz));
    }

    default <S> V insertByDTO(S s) {
        T entity = toEntity(s);
        save(entity);
        return toVO(entity);
    }

    default <S> boolean updateByDTO(S s) {
        return updateById(toEntity(s));
    }

    default V voById(Serializable id) {
        return oneByDTO(new Injector().requiredCondition(Condition.builder().field(ReflectUtil.idField(entityClass())).symbol("=").value(id).build()));
    }

    default <U> U voById(Serializable id, Class<U> clazz) {
        return toTarget(voById(id), clazz);
    }

    default <S> V oneByDTO(S s) {
        List<V> vs = listByDTO(s);
        if (vs == null || vs.isEmpty()) return null;
        if (vs.size() > 1) throw new TooManyResultsException("error query => required one but found " + vs.size());
        return vs.get(0);
    }

    default <S, U> U oneByDTO(S s, Class<U> clazz) {
        return toTarget(oneByDTO(s), clazz);
    }

    default <S> List<V> listByDTO(S s) {
        return doSelect(s, null);
    }

    default <S, U> List<U> listByDTO(S s, Class<U> clazz) {
        return listByDTO(s).stream().map(e -> toTarget(e, clazz)).collect(Collectors.toList());
    }

    default <S> IPage<V> pageByDTO(S s, Long current, Long size) {
        if (current == null) current = 1L;
        if (size == null) size = 10L;
        IPage<V> page = new Page<>(current, size);
        doSelect(s, page);
        return page;
    }

    default <S, U> IPage<U> pageByDTO(S s, Long current, Long size, Class<U> clazz) {
        IPage<V> vp = pageByDTO(s, current, size);
        List<U> us = vp.getRecords().stream().map(e -> toTarget(e, clazz)).collect(Collectors.toList());
        IPage<U> up = new Page<>(vp.getCurrent(), vp.getSize(), vp.getTotal());
        up.setPages(vp.getPages());
        up.setRecords(us);
        return up;
    }

    default <S, U> void exportExcel(S s, OutputStream os, Class<U> clazz, String... includeFields) {
        exportExcel(s, os, clazz, 1L, -1L, includeFields);
    }

    default <S, U> void exportExcel(S s, OutputStream os, Class<U> clazz, Long current, Long size, String... includeFields) {
        List<U> voList = pageByDTO(s, current, size, clazz).getRecords();
        ExcelWriterBuilder builder = EasyExcel.write(os, clazz);
        if (includeFields != null && includeFields.length > 0) {
            builder.includeColumnFieldNames(Arrays.asList(includeFields));
        }
        builder.registerWriteHandler(new LongestMatchColumnWidthStyleStrategy()).sheet().doWrite(voList);
    }

    default <U> void excelTemplate(OutputStream os, Class<U> clazz) {
        EasyExcel.write(os, clazz).registerWriteHandler(new LongestMatchColumnWidthStyleStrategy()).sheet().doWrite(Collections.emptyList());
    }

    default <U> boolean importExcel(InputStream is, Class<U> clazz) {
        List<U> cachedDataList = EasyExcel.read(is).head(clazz).sheet().doReadSync();
        if (cachedDataList == null || cachedDataList.isEmpty()) return false;
        List<T> entityList = cachedDataList.stream().map(this::toEntity).collect(Collectors.toList());
        return saveBatch(entityList);
    }

    <S> List<V> doSelect(S s, IPage<V> page);


}
