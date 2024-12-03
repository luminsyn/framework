package io.github.bootystar.mybatisplus.enhance.core;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import io.github.bootystar.mybatisplus.util.ExcelHelper;
import io.github.bootystar.mybatisplus.util.MybatisPlusReflectHelper;
import org.apache.ibatis.exceptions.TooManyResultsException;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * service
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public interface DynamicService<T, V> extends IService<T> {

    @SuppressWarnings("unchecked")
    default Class<T> classOfEntity() {
        return (Class<T>) Objects.requireNonNull(MybatisPlusReflectHelper.resolveTypeArguments(getClass(), DynamicService.class))[0];
    }

    @SuppressWarnings("unchecked")
    default Class<V> classOfVO() {
        return (Class<V>) Objects.requireNonNull(MybatisPlusReflectHelper.resolveTypeArguments(getClass(), DynamicService.class))[1];
    }

    default T toEntity(Object source) {
        return MybatisPlusReflectHelper.toTarget(source, classOfEntity());
    }

    default V toVO(Object source) {
        return MybatisPlusReflectHelper.toTarget(source, classOfVO());
    }

    default <S> V insertByDTO(S s) {
        T entity = toEntity(s);
        save(entity);
        return toVO(entity);
    }

    default <S> boolean updateByDTO(S s) {
        return updateById(toEntity(s));
    }

    <S> List<V> doSelect(S s, IPage<V> page);

    default V oneById(Serializable id) {
        if (id == null) throw new IllegalArgumentException("id can't be null");
        String idField = MybatisPlusReflectHelper.idFieldPropertyName(classOfEntity());
        if (idField == null) throw new IllegalArgumentException("no id field found in entity");
        ConditionG condition = new ConditionG(idField, SqlKeyword.EQ.keyword, id);
        return oneByDTO(condition);
    }

    default <U> U oneById(Serializable id, Class<U> clazz) {
        return MybatisPlusReflectHelper.toTarget(oneById(id), clazz);
    }

    default <S> V oneByDTO(S s) {
        List<V> vs = listByDTO(s);
        if (vs == null || vs.isEmpty()) return null;
        if (vs.size() > 1) throw new TooManyResultsException("error query => required one but found " + vs.size());
        return vs.get(0);
    }

    default <S, U> U oneByDTO(S s, Class<U> clazz) {
        return MybatisPlusReflectHelper.toTarget(oneByDTO(s), clazz);
    }

    default List<V> listByDTO() {
        return doSelect(null, null);
    }

    default <S> List<V> listByDTO(S s) {
        return doSelect(s, null);
    }

    default <S, U> List<U> listByDTO(S s, Class<U> clazz) {
        return listByDTO(s).stream().map(e -> MybatisPlusReflectHelper.toTarget(e, clazz)).collect(Collectors.toList());
    }

    default <S> IPage<V> pageByDTO(S s, Long current, Long size) {
        if (current == null || current < 1) current = 1L;
        if (size == null) size = 10L;
        IPage<V> page = new Page<>(current, size);
        List<V> vs = doSelect(s, page);
        page.setRecords(vs);
        return page;
    }

    @SuppressWarnings("unchecked")
    default <S, U> IPage<U> pageByDTO(S s, Long current, Long size, Class<U> clazz) {
        IPage<U> vp = (IPage<U>) pageByDTO(s, current, size);
        vp.setRecords(vp.getRecords().stream().map(e -> MybatisPlusReflectHelper.toTarget(e, clazz)).collect(Collectors.toList()));
        return vp;
    }

    default <U> void excelTemplate(OutputStream os, Class<U> clazz) {
        ExcelHelper.write(os, clazz).registerWriteHandler(new LongestMatchColumnWidthStyleStrategy()).sheet().doWrite(Collections.emptyList());
    }

    default <U> boolean excelImport(InputStream is, Class<U> clazz) {
        List<U> cachedDataList = EasyExcel.read(is).head(clazz).sheet().doReadSync();
        if (cachedDataList == null || cachedDataList.isEmpty()) return false;
        List<T> entityList = cachedDataList.stream().map(this::toEntity).collect(Collectors.toList());
        return saveBatch(entityList);
    }

    default <S, U> void excelExport(S s, OutputStream os, Class<U> clazz, String... includeFields) {
        excelExport(s, os, clazz, null, null, includeFields);
    }

    default <S, U> void excelExport(S s, OutputStream os, Class<U> clazz, Long current, Long size, String... includeFields) {
        List<V> voList = current == null && size == null ? listByDTO(s) : pageByDTO(s, current, size).getRecords();
        ExcelHelper.write(os, clazz).includeColumnFieldNames(Arrays.asList(includeFields)).registerWriteHandler(new LongestMatchColumnWidthStyleStrategy()).sheet().doWrite(voList);
    }

}
