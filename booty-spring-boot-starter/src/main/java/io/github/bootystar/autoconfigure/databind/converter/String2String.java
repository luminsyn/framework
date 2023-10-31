package io.github.bootystar.autoconfigure.databind.converter;

import org.springframework.core.convert.converter.Converter;

/**
 * @author booty
 * @since 2023/10/27
 */
public class String2String implements Converter<String, String> {


    @Override
    public String convert(String source) {
        if (source == null || source.isEmpty()) return null;
        return source;
    }
}
