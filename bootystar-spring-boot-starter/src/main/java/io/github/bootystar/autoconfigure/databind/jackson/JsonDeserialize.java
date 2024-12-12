package io.github.bootystar.autoconfigure.databind.jackson;

import com.fasterxml.jackson.annotation.JacksonAnnotationsInside;

import java.lang.annotation.*;
import java.util.function.Function;

/**
 * @author bootystar
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@JacksonAnnotationsInside
public @interface JsonDeserialize {

    Class<? extends Function<String, ?>> value() default Fallback.class;

    class Fallback implements Function<String,Object> {

        @Override
        public Object apply(String o) {
            return o;
        }
    }
}