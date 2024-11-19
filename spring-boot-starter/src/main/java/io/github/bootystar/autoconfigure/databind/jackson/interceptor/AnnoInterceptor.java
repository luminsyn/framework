package io.github.bootystar.autoconfigure.databind.jackson.interceptor;

import com.fasterxml.jackson.databind.introspect.Annotated;
import com.fasterxml.jackson.databind.introspect.NopAnnotationIntrospector;
import io.github.bootystar.autoconfigure.databind.jackson.deserializer.JacksonDeserializer;
import io.github.bootystar.autoconfigure.databind.jackson.serializer.JacksonSerializer;
import io.github.bootystar.autoconfigure.datamask.anno.Mask4JsonDeserialize;
import io.github.bootystar.autoconfigure.datamask.anno.Mask4JsonSerialize;
import lombok.SneakyThrows;
import org.springframework.core.GenericTypeResolver;

import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

/**
 * @author bootystar
 */
public class AnnoInterceptor extends NopAnnotationIntrospector {
    private static final ConcurrentHashMap<Class<?>, JacksonSerializer<?>> SERIALIZER_CACHE = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Class<?>, JacksonDeserializer<?>> DESERIALIZER_CACHE = new ConcurrentHashMap<>();


    @Override
    public Object findSerializer(Annotated am) {
        Mask4JsonSerialize annotation = am.getAnnotation(Mask4JsonSerialize.class);
        if (annotation == null) return null;
        Class<? extends Function<?, String>> clazz = annotation.value();
        JacksonSerializer<?> serializer = SERIALIZER_CACHE.get(clazz);
        if (serializer != null) {
            return serializer;
        }
        return resolveSerializer(clazz);
    }

    @SneakyThrows
    private JacksonSerializer<?> resolveSerializer(Class<? extends Function<?, String>> clazz) {
        boolean anInterface = clazz.isInterface();
        if (!anInterface) {
            Function<?, String> function = clazz.getConstructor().newInstance();
            Class<?>[] classes = GenericTypeResolver.resolveTypeArguments(clazz, Function.class);
            if (classes == null || classes.length == 0) return null;
            Class<?> resolved = classes[0];
            JacksonSerializer<?> serializer = new JacksonSerializer(resolved, function);
            SERIALIZER_CACHE.put(clazz, serializer);
            return serializer;
        }
        return null;
    }

    @Override
    public Object findDeserializer(Annotated am) {
        Mask4JsonDeserialize annotation = am.getAnnotation(Mask4JsonDeserialize.class);
        if (annotation == null) return null;
        Class<? extends Function<String, ?>> clazz = annotation.value();
        JacksonDeserializer<?> deserializer = DESERIALIZER_CACHE.get(clazz);
        if (deserializer != null) {
            return deserializer;
        }
        return resolveDeserializer(clazz);
    }

    @SneakyThrows
    private JacksonDeserializer<?> resolveDeserializer(Class<? extends Function<String, ?>> clazz) {
        boolean anInterface = clazz.isInterface();
        if (!anInterface) {
            Function<String, ?> function = clazz.getConstructor().newInstance();
            Class<?>[] classes = GenericTypeResolver.resolveTypeArguments(clazz, Function.class);
            if (classes == null || classes.length == 0) return null;
            Class<?> resolved = classes[0];
            JacksonDeserializer<?> deserializer = new JacksonDeserializer(resolved, function);
            DESERIALIZER_CACHE.put(clazz, deserializer);
            return deserializer;
        }
        return null;
    }


}
