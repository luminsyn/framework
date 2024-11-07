package io.github.bootystar.mybatisplus.logic.splicing.enums;

import lombok.AllArgsConstructor;

/**
 * SQL操作符
 *
 * @author bootystar
 */
@AllArgsConstructor
public enum Operator {
    NOT("NOT"),
    IN("IN"),
    NOT_IN("NOT IN"),
    LIKE("LIKE"),
    NOT_LIKE("NOT LIKE"),
    EQ("="),
    NE("<>"),
    GT(">"),
    GE(">="),
    LT("<"),
    LE("<="),
    IS_NULL("IS NULL"),
    IS_NOT_NULL("IS NOT NULL"),
//    EXISTS("EXISTS"),
//    NOT_EXISTS("NOT EXISTS"),
//    BETWEEN("BETWEEN"),
//    NOT_BETWEEN("NOT BETWEEN"),
    ;

    public final String keyword;
}
