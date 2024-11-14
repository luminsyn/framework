package io.github.bootystar.mybatisplus.logic.splicing.enums;

import lombok.AllArgsConstructor;

/**
 * SQL连接符
 *
 * @author bootystar
 */
@AllArgsConstructor
public enum Connector {
    AND("AND"),
    OR("OR"),


    ;

    public final String keyword;

}
