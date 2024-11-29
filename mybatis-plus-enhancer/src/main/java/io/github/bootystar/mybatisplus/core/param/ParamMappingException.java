package io.github.bootystar.mybatisplus.core.param;

/**
 * 条件映射异常
 *
 * @author bootystar
 */
public class ParamMappingException extends RuntimeException {

    public ParamMappingException(String message, Object... args) {
        super(String.format(message, args));
    }

}
