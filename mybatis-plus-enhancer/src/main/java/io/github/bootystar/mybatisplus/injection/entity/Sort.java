package io.github.bootystar.mybatisplus.injection.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author bootystar
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Sort {

    /**
     * 字段
     */
    private String field;
    /**
     * 是否正序(默认为false)
     */
    private Boolean asc = false;

}
