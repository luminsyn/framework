package io.github.bootystar.mybatisplus.logic.dynamic;

import java.util.Map;

/**
 * 支持动态查询的实体类
 *
 * @author bootystar
 */
public interface DynamicEntity {

    /**
     * 非实体类对应表字段映射
     * key为实体类属性名
     * value为数据库字段名
     *
     * @return {@link Map }<{@link String }, {@link String }>
     * @author bootystar
     */
    Map<String, String> extraMap();

}
