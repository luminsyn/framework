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


    public Sort newInstance(){
       Sort sort = new Sort();
       sort.setField(this.field);
       sort.setAsc(this.asc);
       return sort;
    }


    public static class ImmutableSort extends Sort{
        public ImmutableSort() {
            throw new UnsupportedOperationException("not support empty constructor");
        }

        public ImmutableSort(String field, Boolean asc) {
            super(field, asc);
        }

        @Override
        public void setField(String field) {
            throw new UnsupportedOperationException("not support set value");
        }

        @Override
        public void setAsc(Boolean asc) {
            throw new UnsupportedOperationException("not support set value");
        }
    }

}
