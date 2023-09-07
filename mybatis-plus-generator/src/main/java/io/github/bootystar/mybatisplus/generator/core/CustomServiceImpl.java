package io.github.bootystar.mybatisplus.generator.core;


import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


/**
 * @Author booty
 * @Date 2023/8/21 9:44
 */
public class CustomServiceImpl<M extends CustomMapper<T,V>, T,V> extends ServiceImpl<M, T> implements CustomService<T,V>{

    @Override
    public T insertByDto(Dto<T> dto) {
        T entity = dto.toEntity();
        super.save(entity);

        return entity;
    }

    @Override
    public boolean insertBatchByDto(Collection<Dto<T> > dtoList) {
        List<T> entityList = dtoList.stream().map(Dto::toEntity).collect(Collectors.toList());
        boolean b = super.saveBatch(entityList);
        return b;
    }

    @Override
    public boolean updateByDto(Dto<T>  dto) {
        T entity = dto.toEntity();
        boolean b = super.updateById(entity);
        return b;
    }

    @Override
    public V getVoById(Serializable id) {
        Map<String, Object> map = new HashMap<>();
        map.put("id",id);
        List<V> records = pageByMap(map, 1L, -1L).getRecords();
        if (records==null || records.isEmpty()) return null;
        if (records.size()>1) throw new RuntimeException("duplicate records for getVoById, expected 1, but got " + records.size());
        return records.get(0);
    }

    @Override
    public List<V> listByDto(Dto<T>  dto) {
        return pageByDto(dto,1L,-1L).getRecords();
    }

    @Override
    public IPage<V> pageByDto(Dto<T>  dto, Long current, Long size) {
        return pageByMap(dto.toMap(), current, size);
    }

    @Override
    public IPage<V> pageByMap(Map<String, Object> map, Long current, Long size) {
        if (current==null || current<1)current=1L;
        if (size==null) size=10L;
        Page<V> page = new Page<>(current, size);
        if (size==-1L) page.setSearchCount(false);
        IPage<V> result = baseMapper.pageByDto(map, page);
        this.voPostProcess(result.getRecords());
        return result;
    }

    protected void voPostProcess(List<V> voList){
        // any children should implement this method if necessary
    }

    @Override
    public void exportExcel(Dto<T> dto, OutputStream os, Class<?> clazz) {
        /*
        response.setContentType("application/vnd.ms-excel");
        response.setCharacterEncoding("utf-8");
        response.setHeader("Access-Control-Expose-Headers","Content-Disposition");
        response.setHeader("Content-disposition", "attachment;filename=" + fileName);
        response.addHeader("Pargam", "no-cache");
        response.addHeader("Cache-Control", "no-cache");
         */
        List<V> voList = listByDto(dto);
        EasyExcel.write(os,clazz).sheet().doWrite(voList);
    }

    @Override
    public void exportExcel(Dto<T> dto, OutputStream os, Class<?> clazz, Collection<String> includeFields) {
        /*
        response.setContentType("application/vnd.ms-excel");
        response.setCharacterEncoding("utf-8");
        response.setHeader("Access-Control-Expose-Headers","Content-Disposition");
        response.setHeader("Content-disposition", "attachment;filename=" + fileName);
        response.addHeader("Pargam", "no-cache");
        response.addHeader("Cache-Control", "no-cache");
         */
        List<V> voList = listByDto(dto);
        EasyExcel.write(os,clazz).includeColumnFieldNames(includeFields).sheet().doWrite(voList);
    }

}
