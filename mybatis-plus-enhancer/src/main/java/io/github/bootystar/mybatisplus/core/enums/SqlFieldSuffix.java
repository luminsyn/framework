package io.github.bootystar.mybatisplus.core.enums;

import lombok.AllArgsConstructor;

/**
 * @author bootystar
 */
@AllArgsConstructor
public enum SqlFieldSuffix {

    EQ("Eq"),
    NE("Ne"),
    GT("Gt"),
    GE("Ge"),
    LT("Lt"),
    LE("Le"),
    LIKE("Like"),
    NOT_LIKE("NotLike"),

    IN("In"),
    NOT_IN("NotIn"),

    IS_NULL("IsNull"),
    IS_NOT_NULL("IsNotNull");

    public final String keyword;

}
