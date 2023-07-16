package io.github.bootystar.mybatisplus.generator.core.entity;

import java.io.Serializable;

/**
 * @Author booty
 * @Date 2023/7/13 11:31
 */
public class SelectDto<T> extends Dto<T> {

    /**
     * 页码
     */
    private Long page = 1L;
    /**
     * 每页大小
     */
    private Long size = 10L;

    /**
     * id
     */
    private Serializable id;

    public Long getPage() {
        return page;
    }

    public void setPage(Long page) {
        this.page = page;
    }

    public Long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
    }

    public Serializable getId() {
        return id;
    }

    public void setId(Serializable id) {
        this.id = id;
    }
}
