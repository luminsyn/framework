package io.github.bootystar.mybatisplus.core.param.normal;

import io.github.bootystar.mybatisplus.core.param.base.ISqlSort;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 排序参数
 *
 * @author bootystar
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SortN implements ISqlSort {

    /**
     * 字段
     */
    protected String field;

    /**
     * 是否倒序(默认为false)
     */
    protected Boolean desc = false;

}
