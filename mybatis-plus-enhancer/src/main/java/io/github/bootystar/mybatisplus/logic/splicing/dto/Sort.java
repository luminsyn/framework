package io.github.bootystar.mybatisplus.logic.splicing.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 排序参数
 *
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
     * 是否倒序(默认为false)
     */
    private Boolean desc = false;

    public static class ImmutableSort extends Sort {
        public ImmutableSort() {
            throw new UnsupportedOperationException("not support empty constructor");
        }

        public ImmutableSort(String field, Boolean desc) {
            super(field, desc);
        }

        @Override
        public void setField(String field) {
            throw new UnsupportedOperationException("not support set value");
        }

        @Override
        public void setDesc(Boolean desc) {
            throw new UnsupportedOperationException("not support set value");
        }
    }

}
