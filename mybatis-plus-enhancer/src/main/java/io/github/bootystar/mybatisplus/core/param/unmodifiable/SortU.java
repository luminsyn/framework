package io.github.bootystar.mybatisplus.core.param.unmodifiable;

import io.github.bootystar.mybatisplus.core.param.base.ISqlSort;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author bootystar
 */
@Getter
@AllArgsConstructor
public class SortU implements ISqlSort {
    /**
     * 字段
     */
    protected String field;

    /**
     * 是否倒序(默认为false)
     */
    protected Boolean desc;

}
