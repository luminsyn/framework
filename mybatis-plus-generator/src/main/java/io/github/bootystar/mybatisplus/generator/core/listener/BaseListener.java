package io.github.bootystar.mybatisplus.generator.core.listener;

import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.read.listener.ReadListener;
import java.util.LinkedList;
import java.util.List;


import com.baomidou.mybatisplus.extension.service.IService;

/**
 * @Author booty
 * @Date 2023/7/26 12:46
 */
public class BaseListener implements ReadListener<Object> {
    private List<Object> cachedDataList = new LinkedList<>();
    private final IService<Object> baseService;


    /**
     * 基地侦听器
     *
     * @param service 服务
     * @return {@code  }
     * @author booty
     * @date 2023/07/26 17:00
     */
    public BaseListener(IService<Object>  service) {
        baseService = service;
    }

    /**
     * 这个每一条数据解析都会来调用
     *
     * @param data    one row value. Is is same as {@link AnalysisContext#readRowHolder()}
     * @param context
     */
    @Override
    public void invoke(Object data, AnalysisContext context) {
        cachedDataList.add(data);
    }


    @Override
    public void doAfterAllAnalysed(AnalysisContext context) {
        // 这里也要保存数据，确保最后遗留的数据也存储到数据库
        saveData();

    }

    /**
     * 加上存储数据库
     */
    private void saveData() {

    }

    private boolean success = false ;
    private String msg ="";

    public boolean isSuccess() {
        return success;
    }

    public String getMsg() {
        return msg;
    }
}
