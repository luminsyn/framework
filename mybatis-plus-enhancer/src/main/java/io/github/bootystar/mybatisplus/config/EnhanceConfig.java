package io.github.bootystar.mybatisplus.config;

import com.baomidou.mybatisplus.generator.config.po.TableField;
import com.baomidou.mybatisplus.generator.config.po.TableInfo;
import io.github.bootystar.mybatisplus.config.base.ConfigBase;
import io.github.bootystar.mybatisplus.config.base.ConfigBaseBuilder;
import io.github.bootystar.mybatisplus.base.injection.dto.InjectionDTO;
import io.github.bootystar.mybatisplus.base.injection.dto.SortDTO;
import lombok.Getter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 父类生成器配置类
 * @author bootystar
 */
@Getter
public class EnhanceConfig extends ConfigBase {
    public EnhanceConfig() {
        super();
        super.injector = true;
    }



    /**
     * 显示 service impl方法
     */
    protected boolean showServiceImplMethod = true;

    /**
     * 显示mapper方法
     */
    protected boolean showMapperMethod = true;

    /**
     * 注入器类名称
     */
    protected String injectorClassSimpleName = InjectionDTO.class.getSimpleName();

    /**
     * 注入器全限定类名
     */
    protected String injectorClassFullName = InjectionDTO.class.getName();

    /**
     * 排序字段名称
     */
    protected String sortClassSimpleName = SortDTO.class.getSimpleName();

    /**
     * 排序字段名称
     */
    protected String sortClassFullName = SortDTO.class.getName();



    @Override
    public Map<String, Object> renderData(TableInfo tableInfo) {
        Map<String, Object> map = super.renderData(tableInfo);
        Map<String, Boolean> orderColumnMap1 = this.getOrderColumnMap();
        List<TableField> fields = tableInfo.getFields();
        HashMap<String, String> column2Property = fields.stream().collect(HashMap::new, (m, e) -> m.put(e.getColumnName(), e.getPropertyName()), HashMap::putAll);
        if (orderColumnMap1 != null && !orderColumnMap1.isEmpty()) {
            ArrayList<SortDTO> sorts = new ArrayList<>();
            for (Map.Entry<String, Boolean> entry : orderColumnMap1.entrySet()) {
                String property = column2Property.get(entry.getKey());
                if (property != null) {
                    SortDTO sort = new SortDTO();
                    sort.setField(property);
                    sort.setDesc(entry.getValue());
                    sorts.add(sort);
                }
            }
            if (!sorts.isEmpty()) {
                map.put("sortsList", sorts);
            }
        }
        return map;
    }

    /**
     * 构造器
     * @author bootystar
     * @since 2023/12/19
     */
    public static class Builder extends ConfigBaseBuilder<EnhanceConfig, Builder> {

        @Override
        protected EnhanceConfig initConfig() {
            return new EnhanceConfig();
        }

        @Override
        protected Builder initBuilder() {
            return this;
        }


        /**
         * 不生成服务impl的父类方法
         *
         * @return {@code U }
         * @author bootystar
         *
         */
        public Builder disableServiceImplOverrideMethod() {
            this.config.showServiceImplMethod = false;
            return this;
        }

        /**
         * 不生成mapper的父类方法
         *
         * @return {@code U }
         * @author bootystar
         *
         */
        public Builder disableMapperOverrideMethod() {
            this.config.showMapperMethod = false;
            return this;
        }
    }

}


