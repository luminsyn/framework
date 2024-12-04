package io.github.bootystar.mybatisplus.enhance.helper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;

import java.util.List;

/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public class LambdaSqlHelperWrapper<T, V> extends AbstractLambdaSqlHelper<T, LambdaSqlHelperWrapper<T, V>> {

    private final DynamicService<T, V> baseService;

    public LambdaSqlHelperWrapper(DynamicService<T, V> baseService) {
        this.baseService = baseService;
    }

    @Override
    protected LambdaSqlHelperWrapper<T, V> returnValue() {
        return this;
    }

    public V one() {
        return baseService.oneByDTO(this);
    }

    public List<V> list() {
        return baseService.listByDTO(this);
    }

    public IPage<V> page(Long current, Long size) {
        return baseService.pageByDTO(this, current, size);
    }


}
