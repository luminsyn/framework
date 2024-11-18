package io.github.bootystar.autoconfigure.datamask.anno;

import com.fasterxml.jackson.annotation.JacksonAnnotationsInside;
import io.github.bootystar.autoconfigure.datamask.fallback.SerializeFallback;

import java.lang.annotation.*;
import java.util.function.Function;

/**
 * @author bootystar
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@JacksonAnnotationsInside
public @interface Mask4JsonSerialize {

    Class<? extends Function<?, String>> value() default SerializeFallback.class;

}