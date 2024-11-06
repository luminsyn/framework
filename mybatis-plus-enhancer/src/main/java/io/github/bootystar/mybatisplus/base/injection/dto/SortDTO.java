package io.github.bootystar.mybatisplus.base.injection.dto;

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
public class SortDTO {

    /**
     * 字段
     */
    private String field;
    /**
     * 是否倒序(默认为false)
     */
    private Boolean desc = false;


    public SortDTO newInstance(){
       SortDTO sort = new SortDTO();
       sort.setField(this.field);
       sort.setDesc(this.desc);
       return sort;
    }


    public static class ImmutableSort extends SortDTO {
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
