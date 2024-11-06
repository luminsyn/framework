package io.github.bootystar.mybatisplus.base.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.base.IBaseService;
import io.github.bootystar.mybatisplus.base.mapper.InjectionMapper;
import io.github.bootystar.mybatisplus.logic.injection.dto.ConditionDTO;
import io.github.bootystar.mybatisplus.logic.injection.dto.InjectionDTO;
import io.github.bootystar.mybatisplus.logic.injection.dto.SortDTO;
import io.github.bootystar.mybatisplus.logic.injection.enums.Connector;
import io.github.bootystar.mybatisplus.logic.injection.enums.Operator;
import io.github.bootystar.mybatisplus.util.ReflectUtil;

import java.util.*;


/**
 * service impl
 *
 * @author bootystar
 */
public abstract class InjectionServiceImpl<M extends InjectionMapper<T, V>, T, V> extends ServiceImpl<M, T> implements IBaseService<T, V> {

    @Override
    public List<String> selectableFields() {
        return new ArrayList<>(ReflectUtil.injectableFieldsMap(entityClass()).keySet());
    }

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s instanceof InjectionDTO) {
            InjectionDTO injection = (InjectionDTO) s;
            return getBaseMapper().listByInjection(injection.safetyinjection(entityClass()), page);
        }
        if (s instanceof InjectionDTO.ImmutableInjection) {
            InjectionDTO.ImmutableInjection<?> injection = (InjectionDTO.ImmutableInjection<?>) s;
            if (entityClass().equals(injection.getEntityClass())) {
                return getBaseMapper().listByInjection((InjectionDTO.ImmutableInjection<T>) injection, page);
            }
            throw new UnsupportedOperationException("not support this type of safety injection: " + injection.getEntityClass());
        }
        InjectionDTO injection = new InjectionDTO();
        if (s != null) {
            if (s instanceof Map) {
                Map<String, Object> map = (Map<String, Object>) s;
                Iterator<Map.Entry<String, Object>> iterator = map.entrySet().iterator();
                List<ConditionDTO> conditions = new ArrayList<>();
                while (iterator.hasNext()) {
                    Map.Entry<String, Object> next = iterator.next();
                    String key = next.getKey();
                    Object value = next.getValue();
                    ConditionDTO dto = new ConditionDTO(Connector.AND.keyword, key, Operator.EQ.keyword, value);
                    conditions.add(dto);
                }
                injection.addConditions(conditions);
            } else {
                injection.requiredConditions(s, Operator.EQ);
            }

        }
        return getBaseMapper().listByInjection(injection.safetyinjection(entityClass()), page);
    }


}
