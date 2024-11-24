package io.github.bootystar.mybatisplus.logic.dynamic;

/**
 * 条件转化异常
 *
 * @author bootystar
 */
public class ConditionConvertException extends RuntimeException {

    public ConditionConvertException(String message, Object... args) {
        super(String.format(message, args));
    }

}
