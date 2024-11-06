package io.github.bootystar.mybatisplus.base.injection;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.base.injection.dto.InjectionDTO;
import io.github.bootystar.mybatisplus.base.injection.dto.SortDTO;
import io.github.bootystar.mybatisplus.base.injection.enums.Operator;

import java.util.List;


/**
 * service impl
 *
 * @author bootystar
 */
public abstract class InjectionServiceImpl<M extends InjectionMapper<T, V>, T, V> extends ServiceImpl<M, T> implements InjectionService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s instanceof InjectionDTO) {
            InjectionDTO injector = (InjectionDTO) s;
            if (injector.getSorts() == null || injector.getSorts().isEmpty()) {
                injector.setSorts(defaultSorts());
            }
            return getBaseMapper().listByInjector(injector.safetyInjector(entityClass()), page);
        }
        if (s instanceof InjectionDTO.SafetyInjector) {
            InjectionDTO.SafetyInjector<?> injector = (InjectionDTO.SafetyInjector<?>) s;
            if (entityClass().equals(injector.getEntityClass())) {
                return getBaseMapper().listByInjector((InjectionDTO.SafetyInjector<T>) injector, page);
            }
            throw new UnsupportedOperationException("not support this type of safety injector: " + injector.getEntityClass());
        }
        InjectionDTO injector = new InjectionDTO();
        if (s != null) {
            injector.requiredConditions(s, Operator.EQ);
        }
        if (injector.getSorts() == null || injector.getSorts().isEmpty()) {
            injector.setSorts(defaultSorts());
        }
        return getBaseMapper().listByInjector(injector.safetyInjector(entityClass()), page);
    }

    /**
     * 默认排序
     *
     * @return {@link List }<{@link SortDTO }>
     * @author bootystar
     */
    protected abstract List<SortDTO> defaultSorts();


}
