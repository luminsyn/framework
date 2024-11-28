package io.github.bootystar.mybatisplus.core.param;

/**
 * @author bootystar
 */
public class UnmodifiableSort extends Sort {

    public UnmodifiableSort(String field, Boolean desc) {
        this.field = field;
        this.desc = desc;
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
