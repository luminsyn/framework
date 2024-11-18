package io.github.bootystar.autoconfigure.datamask.anno;

import io.github.bootystar.autoconfigure.datamask.fallback.SerializeFallback;

import java.lang.annotation.*;
import java.util.function.Function;

/**
 *
 * @author bootystar
 */
@Target({ElementType.FIELD,ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Mask4Db {

    Class<? extends Function<?, ?>> value() default SerializeFallback.class;

}