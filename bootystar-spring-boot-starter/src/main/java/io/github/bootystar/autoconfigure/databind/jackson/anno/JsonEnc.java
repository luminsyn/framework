package io.github.bootystar.autoconfigure.databind.jackson.anno;

import com.fasterxml.jackson.annotation.JacksonAnnotationsInside;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.github.bootystar.autoconfigure.databind.jackson.deserializer.JsonFieldDeserializer;
import io.github.bootystar.autoconfigure.databind.jackson.serializer.JsonFieldSerializer;

import java.lang.annotation.*;
import java.util.function.Function;

/**
 * @author bootystar
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@JacksonAnnotationsInside
@JsonSerialize(using = JsonFieldSerializer.class)
@JsonDeserialize(using = JsonFieldDeserializer.class)
public @interface JsonEnc {

    Class<? extends Function<?, String>> serialize() default Fallback.class;

    Class<? extends Function<String, ?>> deserialize() default Fallback.class;

    class Fallback implements Function<String, String> {
        @Override
        public String apply(String s) {
            return s;
        }
    }


}