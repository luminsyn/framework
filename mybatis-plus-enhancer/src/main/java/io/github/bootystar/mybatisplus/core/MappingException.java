package io.github.bootystar.mybatisplus.core;

/**
 * 条件映射异常
 *
 * @author bootystar
 */
public class MappingException extends RuntimeException {

    public MappingException(String message, Object... args) {
        super(String.format(message, args));
    }

}
