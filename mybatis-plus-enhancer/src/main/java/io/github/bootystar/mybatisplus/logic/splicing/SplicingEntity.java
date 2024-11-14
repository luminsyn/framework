package io.github.bootystar.mybatisplus.logic.splicing;

import java.util.Map;

/**
 * 可SQL拼接的实体类
 *
 * @author bootystar
 */
public interface SplicingEntity {

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
