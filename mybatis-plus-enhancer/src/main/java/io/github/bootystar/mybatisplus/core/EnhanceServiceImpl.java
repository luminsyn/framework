package io.github.bootystar.mybatisplus.core;


import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.exception.ExcelAnalysisException;
import com.alibaba.excel.exception.ExcelDataConvertException;
import com.alibaba.excel.read.listener.ReadListener;
import com.alibaba.excel.write.builder.ExcelWriterBuilder;
import com.alibaba.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.injection.Condition;
import io.github.bootystar.mybatisplus.injection.Injector;
import io.github.bootystar.mybatisplus.util.ReflectUtil;
import org.apache.ibatis.exceptions.TooManyResultsException;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.util.*;
import java.util.stream.Collectors;


/**
 * service impl
 *
 * @author bootystar
 */
public abstract class EnhanceServiceImpl<M extends EnhanceMapper<T, V>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    public <S> V insertByDTO(S s) {
        T entity = toEntity(s);
        super.save(entity);
        return toVO(entity);
    }

    @Override
    public <S> boolean updateByDTO(S s) {
        T entity = toEntity(s);
        return super.updateById(entity);
    }

    @Override
    public V voById(Serializable id) {
        String keyProperty = ReflectUtil.idField(getEntityClass());
        return oneByDTO(new Injector().requiredCondition(Condition.builder().field(keyProperty).symbol("=").value(id).build()));
    }

    @Override
    public <U> U voById(Serializable id, Class<U> clazz) {
        V vo = voById(id);
        return toTarget(vo, clazz);
    }

    @Override
    public <S> V oneByDTO(S s) {
        List<V> vs = listByDTO(s);
        if (vs == null || vs.isEmpty()) {
            return null;
        }
        if (vs.size() > 1) {
            throw new TooManyResultsException("error query => required one but found " + vs.size());
        }
        return vs.get(0);
    }

    @Override
    public <S, U> U oneByDTO(S s, Class<U> clazz) {
        return toTarget(oneByDTO(s), clazz);
    }

    @Override
    public <S> List<V> listByDTO(S s) {
        return doSelect(s, null);
    }

    @Override
    public <S, U> List<U> listByDTO(S s, Class<U> clazz) {
        return listByDTO(s).stream().map(e -> toTarget(e, clazz)).collect(Collectors.toList());
    }

    @Override
    public <S> IPage<V> pageByDTO(S s, Long current, Long size) {
        if (current == null || current < 1) {
            current = 1L;
        }
        if (size == null) {
            size = 10L;
        }
        Page<V> page = new Page<>(current, size);
        doSelect(s, page);
        return page;
    }

    @Override
    public <S, U> IPage<U> pageByDTO(S s, Long current, Long size, Class<U> clazz) {
        IPage<V> vp = pageByDTO(s, current, size);
        List<U> us = vp.getRecords().stream().map(e -> toTarget(e, clazz)).collect(Collectors.toList());
        IPage<U> up = new Page<>(vp.getCurrent(),vp.getSize(),vp.getTotal());
        up.setPages(vp.getPages());
        up.setRecords(us);
        return up;
    }

    protected <S> List<V> doSelect(S s, IPage<V> page) {
        if (s instanceof Injector) {
            Injector injector = (Injector) s;
            List<V> vs = baseMapper.listByCondition(injector.init(getEntityClass()), page);
            voPostProcess(vs);
            if (page != null) {
                page.setRecords(vs);
            }
            return vs;
        }
        List<V> vs = baseMapper.listByMap(toMap(s), page);
        voPostProcess(vs);
        if (page != null) {
            page.setRecords(vs);
        }
        return vs;
    }

    protected void voPostProcess(List<V> dataList) {

    }

    @Override
    public <S, U> void exportExcel(S s, OutputStream os, Class<U> clazz, String... includeFields) {
        exportExcel(s, os, clazz, 1L, -1L, includeFields);
    }

    @Override
    public <S, U> void exportExcel(S s, OutputStream os, Class<U> clazz, Long current, Long size, String... includeFields) {
        List<U> voList = pageByDTO(s, current, size, clazz).getRecords();
        ExcelWriterBuilder builder = EasyExcel.write(os, clazz);
        if (includeFields != null && includeFields.length > 0) {
            builder.includeColumnFieldNames(Arrays.asList(includeFields));
        }
        builder.registerWriteHandler(new LongestMatchColumnWidthStyleStrategy()).sheet().doWrite(voList);
    }

    @Override
    public <U> void excelTemplate(OutputStream os, Class<U> clazz) {
        EasyExcel.write(os, clazz).registerWriteHandler(new LongestMatchColumnWidthStyleStrategy()).sheet().doWrite(Collections.emptyList());
    }

    @Override
    public <U> boolean importExcel(InputStream is, Class<U> clazz) {
        List<U> cachedDataList = importPreHandle(is, clazz);
        if (cachedDataList == null || cachedDataList.isEmpty()) return false;
        List<T> entityList = importPostHandle(cachedDataList);
        return super.saveBatch(entityList);
    }

    protected <U> List<U> importPreHandle(InputStream is, Class<U> clazz) {
        List<U> cachedDataList = new ArrayList<>(128);
        ReadListener<U> listener = new ReadListener<U>() {
            @Override
            public void invoke(U data, AnalysisContext context) {
                cachedDataList.add(data);
            }

            @Override
            public void doAfterAllAnalysed(AnalysisContext analysisContext) {

            }
        };
        try {
            EasyExcel.read(is, clazz, listener).sheet().doRead();
        } catch (ExcelAnalysisException e) {
            ExcelDataConvertException excelDataConvertException = (ExcelDataConvertException) e.getCause();
            String msg = String.format("第%s行，第%s列数据格式不正确：%s", excelDataConvertException.getRowIndex() + 1, excelDataConvertException.getColumnIndex(), excelDataConvertException.getCellData());
            throw new RuntimeException(msg);
        }
        return cachedDataList;
//        return EasyExcel.read(is).head(clazz).sheet().doReadSync();
    }

    protected <U> List<T> importPostHandle(List<U> cachedDataList) {
        return cachedDataList.stream().map(this::toEntity).collect(Collectors.toList());
    }


    @Override
    public T toEntity(Object source) {
        ParameterizedType pt = (ParameterizedType) getClass().getGenericSuperclass();
        Class<T> clazz = (Class<T>) pt.getActualTypeArguments()[1];
        return toTarget(source, clazz);
//        return toTarget(source, super.getEntityClass());
    }

    @Override
    public V toVO(Object source) {
        ParameterizedType pt = (ParameterizedType) getClass().getGenericSuperclass();
        Class<V> clazz = (Class<V>) pt.getActualTypeArguments()[2];
        return toTarget(source, clazz);
    }

    @Override
    public Collection<String> injectableFields() {
        return ReflectUtil.injectableFieldsMap(super.getEntityClass()).keySet();
    }

    protected <U> U toTarget(Object source, Class<U> clazz) {
        if (source == null || clazz == null) {
            return null;
        }
        if (clazz.isInstance(source)) {
            return (U) source;
        }
        return ReflectUtil.copyProperties(source, ReflectUtil.newInstance(clazz));
    }

    protected Map<String, Object> toMap(Object source) {
        return ReflectUtil.objectToMap(source);
    }
}
