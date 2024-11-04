package io.github.bootystar.mybatisplus.injection.enums;

import lombok.AllArgsConstructor;

/**
 * @author bootystar
 */
@AllArgsConstructor
public enum Connector {
    AND("AND"),
    OR("OR"),


    ;

    public final String keyword;

}
