package io.github.bootystar.mybatisplus.enhance.helper;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.LambdaMeta;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import org.apache.ibatis.reflection.property.PropertyNamer;

import java.util.List;

/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public class LambdaSqlHelper<T, V, F extends SFunction<T, ?>> {

    private final DynamicService<T, V> baseService;
    private final SqlHelper sqlHelper = new SqlHelper();

    protected String getFieldName(F func) {
        LambdaMeta meta = LambdaUtils.extract(func);
        return PropertyNamer.methodToProperty(meta.getImplMethodName());
    }

    public LambdaSqlHelper(DynamicService<T, V> baseService) {
        this.baseService = baseService;
    }

    V one() {
        return baseService.oneByDTO(sqlHelper);
    }

    List<V> list() {
        return baseService.listByDTO(sqlHelper);
    }

    IPage<V> page(Long current, Long size) {
        return baseService.pageByDTO(sqlHelper, current, size);
    }

    public LambdaSqlHelper<T, V, F> eq(F func, Object value) {
        sqlHelper.addConditions(new ConditionG(getFieldName(func), SqlKeyword.EQ.keyword, value));
        return this;
    }


}
