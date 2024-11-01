package io.github.bootystar.mybatisplus.injection;

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
     * 是否正序
     */
    private Boolean asc;

    public Boolean getAsc() {
        if (asc == null) return false;
        return asc;
    }

}
