package io.github.bootystar.mybatisplus.generator.core;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.generator.core.entity.InsertDto;
import io.github.bootystar.mybatisplus.generator.core.entity.SelectDto;
import io.github.bootystar.mybatisplus.generator.core.entity.UpdateDto;
import io.github.bootystar.mybatisplus.generator.core.entity.Vo;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import com.alibaba.excel.EasyExcel;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.UUID;
import java.io.UnsupportedEncodingException;

import java.io.Serializable;
import java.util.List;


/**
 * service默认实现
 * @Author booty
 * @Date 2023/7/13 10:41
 */
public class CustomServiceImp<M extends CustomMapper<T, P>, T, P extends SelectDto<T>> extends ServiceImpl<M, T> implements CustomService<T,P> {


    @Override
    public T insertByDto(InsertDto<T> dto) {
        T t = dto.toBean();
        save(t);
        return t;
    }

    @Override
    public Boolean updateByDto(UpdateDto<T> dto) {
        T t = dto.toBean();
        return updateById(t);
    }

    @Override
    public Vo<T> getVoById(Serializable id) {
        SelectDto<T> dto = new SelectDto<T>();
        dto.setId(id);
        dto.setSize(1L);
        IPage<Vo<T>> page = pageByDto((P) dto);
        List<Vo<T>> records = page.getRecords();
        return records==null|| records.size()==0?null:records.get(0);
    }

    @Override
    public IPage<Vo<T>> pageByDto(P dto) {
        Page<Vo<T>> page = new Page<>(dto.getPage(), dto.getSize());
        return baseMapper.pageByDto(dto,page);
    }


    @Override
    public void exportExcel(P dto,Class<? extends Vo<T>> clazz) {
        dto.setPage(1L);
        dto.setSize(-1L);
        IPage<Vo<T>> page = pageByDto(dto);
        List<Vo<T>> records = page.getRecords();
        String uuid = UUID.randomUUID().toString().replace("-", "");
        try (ServletOutputStream outputStream = getResponse4excel(uuid).getOutputStream()){
            EasyExcel.write(outputStream,clazz).sheet().doWrite(records);
        }catch (Exception e){
            e.printStackTrace();
        }
    }




    public static String convertFileName(String fileName) {
        try {
            return new String(URLEncoder.encode(fileName, "UTF-8").getBytes(), StandardCharsets.ISO_8859_1);
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
            throw new RuntimeException(e.getMessage());
        }
    }



    public static HttpServletResponse getResponse4excel(String fileName){
        ServletRequestAttributes requestAttributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        HttpServletResponse response = requestAttributes.getResponse();
        response.setContentType("application/vnd.ms-excel");
        response.setCharacterEncoding("utf-8");
        response.setHeader("Access-Control-Expose-Headers","Content-Disposition");
        response.setHeader("Content-disposition", "attachment;filename=" + fileName);
        response.addHeader("Pargam", "no-cache");
        response.addHeader("Cache-Control", "no-cache");
        return response;
    }

}
