package io.github.bootystar.mybatisplus.injection;

import lombok.Data;

/**
 * @author bootystar
 */
@Data
public class Sort {

    /**
     * 字段
     */
    private String field;
    /**
     * 是否正序
     */
    private Boolean asc;
}
