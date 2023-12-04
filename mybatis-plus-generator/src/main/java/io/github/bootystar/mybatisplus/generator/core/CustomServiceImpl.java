package io.github.bootystar.mybatisplus.generator.core;


import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.exception.ExcelAnalysisException;
import com.alibaba.excel.exception.ExcelDataConvertException;
import com.alibaba.excel.read.listener.ReadListener;
import com.alibaba.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.util.*;
import java.util.stream.Collectors;


/**
 * @author booty
 * @since 2023/8/21 9:44
 */
public abstract class CustomServiceImpl<M extends CustomMapper<T,V>,T,V> extends ServiceImpl<M, T> implements CustomService<T,V> {

    @Override
    public <S> V insertByDTO(S DTO) {
        T entity = this.toEntity(DTO);
        super.save(entity);
        return this.toVO(entity);
    }

    @Override
    public <S> boolean insertBatchByDTO(Collection<S> DTOList) {
        List<T> entityList = DTOList.stream().map(this::toEntity).collect(Collectors.toList());
        return super.saveBatch(entityList);
    }

    @Override
    public <S> boolean updateByDTO(S DTO) {
        T entity = this.toEntity(DTO);
        return super.updateById(entity);
    }

    @Override
    public V voById(Serializable id) {
        HashMap<String, Object> map = new HashMap<>();
        map.put("primaryKey", id);
        List<V> vs = this.listByDTO(map);
        if (vs == null || vs.size()==0 ) {
            return null;
        }
        if(vs.size() > 1) {
            throw new RuntimeException("error query => required one but found"+vs.size());
        }
        return vs.get(0);
    }

    @Override
    public <U> U voById(Serializable id, Class<U> clazz) {
        V vo = voById(id);
        return this.toTarget(vo, clazz);
    }

    @Override
    public <S> V oneByDTO(S DTO) {
        List<V> vs = listByDTO(DTO);
        if (vs == null || vs.size()==0 ) {
            return null;
        }
        if(vs.size() > 1) {
            throw new RuntimeException("error query => required one but found"+vs.size());
        }
        return vs.get(0);
    }

    @Override
    public <S, U> U oneByDTO(S DTO, Class<U> clazz) {
        List<U> vs = listByDTO(DTO,clazz);
        if (vs == null || vs.size()==0 ) {
            return null;
        }
        if(vs.size() > 1) {
            throw new RuntimeException("error query => required one but found"+vs.size());
        }
        return vs.get(0);
    }

    @Override
    public <S> List<V> listByDTO(S DTO) {
        List<V> voList = this.baseMapper.listByMap(this.toMap(DTO), null);
        this.voPostProcess(voList);
        return voList;
    }

    @Override
    public <S,U> List<U> listByDTO(S DTO, Class<U> clazz) {
        return this.listByDTO(DTO).stream().map(e->this.toTarget(e, clazz)).collect(Collectors.toList());
    }

    @Override
    public <S> IPage<V> pageByDTO(S DTO, Long current, Long size) {
        if (current == null || current<1) {
            current=1L;
        }
        if (size == null) {
            size=10L;
        }
        Page<V> page = new Page<>(current, size);
        List<V> voList = this.baseMapper.listByMap(this.toMap(DTO), page);
        this.voPostProcess(voList);
        page.setRecords(voList);
        return page;
    }

    @Override
    public <S,U> IPage<U> pageByDTO(S DTO, Long current, Long size, Class<U> clazz) {
        IPage<V> viPage = this.pageByDTO(DTO, current, size);
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
    public <S,U> void exportExcel(S DTO, OutputStream os, Class<U> clazz) {
        List<U> voList = listByDTO(DTO,clazz);
        EasyExcel.write(os, clazz).sheet().doWrite(voList);
    }

    @Override
    public <S,U> void exportExcel(S DTO, OutputStream os, Class<U> clazz, Collection<String> includeFields) {
        List<U> voList = listByDTO(DTO,clazz);
        EasyExcel.write(os, clazz).includeColumnFieldNames(includeFields).sheet().doWrite(voList);
    }

    @Override
    public <U> boolean importExcel(InputStream is, Class<U> clazz) {
        List<U> cachedDataList = this.processImportData(is,clazz);
        if (cachedDataList == null || cachedDataList.isEmpty()) return false;
        List<T> entityList = this.processImportData(cachedDataList);
        return super.saveBatch(entityList);
    }

    @Override
    public <U> void exportTemplate(OutputStream os, Class<U> clazz) {
        EasyExcel.write(os, clazz).sheet().doWrite(Collections.emptyList());
    }

    protected <U> List<T> processImportData(List<U> cachedDataList) {
        List<T> entityList = cachedDataList.stream().map(this::toEntity).collect(Collectors.toList());
        // any children should implement this method if necessary
        return entityList;
    }

    protected <U> List<U> processImportData(InputStream is, Class<U> clazz) {
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
            String msg = String.format("第%s行，第%s列数据格式不正确：%s", excelDataConvertException.getRowIndex() + 1, excelDataConvertException.getColumnIndex(), excelDataConvertException.getCellData());
            throw new RuntimeException(msg);
        }
        return cachedDataList;
    }

    @Override
    public T toEntity(Object source) {
        T t = null;
        try {
            ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
            Class<T> clazz = (Class) pt.getActualTypeArguments()[1];
            t = toTarget(source, clazz);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return t;
    }

    @Override
    public V toVO(Object source) {
        V v = null;
        try {
            ParameterizedType pt = (ParameterizedType) this.getClass().getGenericSuperclass();
            Class<V> clazz = (Class) pt.getActualTypeArguments()[2];
            v = toTarget(source, clazz);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return v;
    }


    @Override
    public Map<String, Object> toMap(Object source) {
        if (source==null){
            return new HashMap<>();
        }
        if (source instanceof Map) {
            return new HashMap<>((Map) source);
        }
        Map<String, Object> map = new LinkedHashMap<>();
        Class<?> clazz = source.getClass();
        Field[] fields = clazz.getDeclaredFields();
        try {
            for (Field field : fields) {
                if (unAccessibleFiled(field)){
                    continue;
                }
                String key = field.getName();
                Object value = field.get(source);
                if (value != null) {
                    map.put(key, value);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return map;
    }

    @Override
    public void copyProp(Object source, Object target) {
        if (source==null || target==null){
            return;
        }
        Map<?, ?> map = this.toMap(source);
        Field[] declaredFields = target.getClass().getDeclaredFields();
        for (Field field : declaredFields) {
            if (unAccessibleFiled(field)){
                continue;
            }
            String name = field.getName();
            Object o = map.get(name);
            if (o != null) {
                try {
                    field.set(target, o);
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    @Override
    public <U> U toTarget(Object source, Class<U> clazz) {
        U target = null;
        try {
            if (clazz==null){
                return null;
            }
            target = clazz.newInstance();
            this.copyProp(source, target);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        return target;
    }


    private static boolean unAccessibleFiled(Field field){
        field.setAccessible(true);
        int modifiers = field.getModifiers();
        if (Modifier.isFinal(modifiers)){
            return true;
        }
        if (Modifier.isStatic(modifiers)){
            return true;
        }
        if (Modifier.isNative(modifiers)){
            return true;
        }
        return false;
    }
}
