package io.github.bootystar.mybatisplus.injection;

/**
 * @author bootystar
 */
public class AntiInjectException extends RuntimeException{
    public AntiInjectException(String message) {
        super(message);
    }
}
