package io.github.bootystar.autoconfigure.datamask.fallback;


import java.util.function.Function;

/**
 * @author bootystar
 */
public class DeserializeFallback implements Function<String,Object> {

    @Override
    public Object apply(String o) {
        return String.valueOf(o);
    }

    private DeserializeFallback() {
    }

    public static DeserializeFallback getInstance() {
        return SingletonFallback.INSTANCE.getInstance();
    }

    private enum SingletonFallback {
        INSTANCE;

        private final DeserializeFallback fallback;

        SingletonFallback() {
            fallback = new DeserializeFallback();
        }

        public DeserializeFallback getInstance() {
            return fallback;
        }
    }

}
