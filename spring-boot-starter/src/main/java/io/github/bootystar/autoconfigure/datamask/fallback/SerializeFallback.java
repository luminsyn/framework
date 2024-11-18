package io.github.bootystar.autoconfigure.datamask.fallback;


import java.util.function.Function;

/**
 * @author bootystar
 */
public class SerializeFallback implements Function<Object,String> {

    @Override
    public String apply(Object o) {
        return String.valueOf(o);
    }

    private SerializeFallback() {
    }

    public static SerializeFallback getInstance() {
        return SingletonFallback.INSTANCE.getInstance();
    }

    private enum SingletonFallback {
        INSTANCE;

        private final SerializeFallback fallback;

        SingletonFallback() {
            fallback = new SerializeFallback();
        }

        public SerializeFallback getInstance() {
            return fallback;
        }
    }

}
