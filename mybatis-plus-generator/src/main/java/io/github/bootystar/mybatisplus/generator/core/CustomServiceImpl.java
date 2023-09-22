package io.github.bootystar.mybatisplus.generator.core;


import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.exception.ExcelAnalysisException;
import com.alibaba.excel.exception.ExcelDataConvertException;
import com.alibaba.excel.read.listener.ReadListener;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;


/**
 * @Author booty
 * @Date 2023/8/21 9:44
 */
public abstract class CustomServiceImpl<T,V,M extends CustomMapper<T,V>> extends ServiceImpl<M, T> implements CustomService<T,V> {

    @Override
    public <S> V insertByDto(S dto) {
        T entity = this.toEntity(dto);
        super.save(entity);
        return this.toVo(entity);
    }

    @Override
    public <S> boolean insertBatchByDto(Collection<S> dtoList) {
        List<T> entityList = dtoList.stream().map(this::toEntity).collect(Collectors.toList());
        return super.saveBatch(entityList);
    }

    @Override
    public <S> boolean updateByDto(S dto) {
        T entity = this.toEntity(dto);
        return super.updateById(entity);
    }

    @Override
    public V getVoById(Serializable id) {
        HashMap<String, Object> map = new HashMap<>();
        map.put("primaryKey", id);
        List<V> vs = this.listByDto(map);
        if (vs == null || vs.size()==0 ) {
            return null;
        }
        if(vs.size() > 1) {
            throw new RuntimeException("error query => required one but found"+vs.size());
        }
        return vs.get(0);
    }

    @Override
    public <U> U getVoById(Serializable id, Class<U> clazz) {
        V vo = getVoById(id);
        return this.toTarget(vo, clazz);
    }

    @Override
    public <S> List<V> listByDto(S dto) {
        List<V> voList = this.baseMapper.listByDto(this.toMap(dto), null);
        this.voPostProcess(voList);
        return voList;
    }

    @Override
    public <S,U> List<U> listByDto(S dto, Class<U> clazz) {
        return this.listByDto(dto).stream().map(e->this.toTarget(e, clazz)).collect(Collectors.toList());
    }

    @Override
    public <S> IPage<V> pageByDto(S dto, Long current, Long size) {
        if (current == null || current<1) {
            current=1L;
        }
        if (size == null) {
            size=10L;
        }
        Page<V> page = new Page<>(current, size);
        List<V> voList = this.baseMapper.listByDto(this.toMap(dto), page);
        this.voPostProcess(voList);
        page.setRecords(voList);
        return page;
    }

    @Override
    public <S,U> IPage<U> pageByDto(S dto, Long current, Long size, Class<U> clazz) {
        IPage<V> viPage = this.pageByDto(dto, current, size);
        List<U> voList = viPage.getRecords().stream().map(e->this.toTarget(e,clazz)).collect(Collectors.toList());
        Page<U> uPage = new Page<>();
        uPage.setCurrent(viPage.getCurrent());
        uPage.setSize(viPage.getSize());
        uPage.setTotal(viPage.getTotal());
        uPage.setPages(viPage.getPages());
        uPage.setRecords(voList);
        return uPage;
    }

    protected void voPostProcess(List<V> dataList) {
        if (dataList==null) {
            return;
        }
        // any children should implement this method if necessary
    }

    @Override
    public <S,U> void exportExcel(S dto, OutputStream os, Class<U> clazz) {
        List<U> voList = listByDto(dto,clazz);
        EasyExcel.write(os, clazz).sheet().doWrite(voList);
    }

    @Override
    public <S,U> void exportExcel(S dto, OutputStream os, Class<U> clazz, Collection<String> includeFields) {
        List<U> voList = listByDto(dto,clazz);
        EasyExcel.write(os, clazz).includeColumnFieldNames(includeFields).sheet().doWrite(voList);
    }

    @Override
    public <U> boolean importExcel(InputStream is, Class<U> clazz) {
        List<U> cachedDataList = new LinkedList<>();
        ReadListener<U> listener = new ReadListener<U>() {
            @Override
            public void invoke(U data, AnalysisContext context) {
                cachedDataList.add(data);
            }

            @Override
            public void doAfterAllAnalysed(AnalysisContext context) {
                //do nothing
            }
        };
        try {
            EasyExcel.read(is, clazz, listener).sheet().doRead();
        } catch (ExcelAnalysisException e) {
            ExcelDataConvertException excelDataConvertException = (ExcelDataConvertException) e.getCause();
            String msg = String.format("第%s行，第%s列数据格式不正确：%s", excelDataConvertException.getRowIndex() + 1, excelDataConvertException.getColumnIndex() + 1, excelDataConvertException.getCellData());
            throw new RuntimeException(msg);
        }
        List<T> result = this.processImportData(cachedDataList);
        if (result == null || result.isEmpty()) return false;
        return super.saveBatch(result);
    }

    protected <U> List<T> processImportData(List<U> cachedDataList) {
        List<T> entityList = cachedDataList.stream().map(this::toEntity).collect(Collectors.toList());
        // any children should implement this method if necessary
        return entityList;
    }

}
