package io.github.bootystar.mybatisplus.injection;

/**
 * @author booty
 */
public class AntiInjectException extends RuntimeException{
    public AntiInjectException(String message) {
        super(message);
    }
}
