package io.github.bootystar.mybatisplus.base.impl;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.base.GenericMapper;
import io.github.bootystar.mybatisplus.base.GenericService;
import io.github.bootystar.mybatisplus.easyexcel.EasyExcelConverterTool;
import io.github.bootystar.mybatisplus.logic.splicing.dto.Condition;
import io.github.bootystar.mybatisplus.logic.splicing.dto.Splicer;
import io.github.bootystar.mybatisplus.logic.splicing.enums.Connector;
import io.github.bootystar.mybatisplus.logic.splicing.enums.Operator;
import io.github.bootystar.mybatisplus.util.ReflectUtil;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;


/**
 * SQL拼接实现类
 *
 * @author bootystar
 */
public abstract class SplicingServiceImpl<M extends GenericMapper<T, V>, T, V> extends ServiceImpl<M, T> implements GenericService<T, V> {

    static {
        EasyExcelConverterTool.init();
    }

    @Override
    public List<String> selectableFields() {
        return new ArrayList<>(ReflectUtil.injectableFieldsMap(entityClass()).keySet());
    }

    @Override
    @SuppressWarnings("unchecked")
    public <S> List<V> doSelect(S s, IPage<V> page) {
        if (s instanceof Splicer) {
            Splicer splicer = (Splicer) s;
            return getBaseMapper().listByGeneric(splicer.immutableSplicer(entityClass()), page);
        }
        if (s instanceof Splicer.ImmutableSplicer) {
            Splicer.ImmutableSplicer<?> splicer = (Splicer.ImmutableSplicer<?>) s;
            if (entityClass().equals(splicer.getEntityClass())) {
                return getBaseMapper().listByGeneric((Splicer.ImmutableSplicer<T>) splicer, page);
            }
            throw new UnsupportedOperationException("not support this type of safety splicer: " + splicer.getEntityClass());
        }
        Splicer splicer = new Splicer();
        if (s != null) {
            if (s instanceof Map) {
                Map<String, Object> map = (Map<String, Object>) s;
                Iterator<Map.Entry<String, Object>> iterator = map.entrySet().iterator();
                List<Condition> conditions = new ArrayList<>();
                while (iterator.hasNext()) {
                    Map.Entry<String, Object> next = iterator.next();
                    String key = next.getKey();
                    Object value = next.getValue();
                    Condition dto = new Condition(Connector.AND.keyword, key, Operator.EQ.keyword, value);
                    conditions.add(dto);
                }
                splicer.addConditions(conditions);
            } else {
                splicer.requiredConditions(s, Operator.EQ);
            }

        }
        return getBaseMapper().listByGeneric(splicer.immutableSplicer(entityClass()), page);
    }


}
