package io.github.bootystar.autoconfigure.datamask.fallback;


import java.util.function.Function;

/**
 * @author bootystar
 */
public class DeserializeFallback implements Function<String,Object> {

    @Override
    public Object apply(String o) {
        return o;
    }

}
