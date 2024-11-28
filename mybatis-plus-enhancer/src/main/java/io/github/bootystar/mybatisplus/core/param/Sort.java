package io.github.bootystar.mybatisplus.core.param;

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
public class Sort {

    /**
     * 字段
     */
    protected String field;

    /**
     * 是否倒序(默认为false)
     */
    protected Boolean desc = false;

}
