package io.github.bootystar.mybatisplus.core;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.injection.Injector;
import io.github.bootystar.mybatisplus.injection.enums.Operator;

import java.util.List;


/**
 * service impl
 *
 * @author bootystar
 */
public abstract class EnhanceServiceImpl<M extends EnhanceMapper<T, V>, T, V> extends ServiceImpl<M, T> implements EnhanceService<T, V> {

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s == null) return getBaseMapper().listByInjector(null,page);
        if (s instanceof Injector){
            Injector injector = (Injector) s;
            return getBaseMapper().listByInjector(injector.safetyInjector(entityClass()),page);
        }
        if (s instanceof Injector.SafetyInjector){
            Injector.SafetyInjector<?> injector = (Injector.SafetyInjector<?>) s;
            if (entityClass().equals(injector.getEntityClass())) {
                return getBaseMapper().listByInjector((Injector.SafetyInjector<T>) injector,page);
            }
            throw new UnsupportedOperationException("not support this type of safety injector: " + injector.getEntityClass());
        }
        Injector injector = new Injector();
        injector.requiredConditions(s, Operator.EQ);
        return getBaseMapper().listByInjector(injector.safetyInjector(entityClass()),page);
    }



}
