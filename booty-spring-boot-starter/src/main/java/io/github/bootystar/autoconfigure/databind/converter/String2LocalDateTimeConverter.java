package io.github.bootystar.autoconfigure.databind.converter;

import io.github.bootystar.autoconfigure.databind.constant.DateConst;
import org.springframework.core.convert.converter.Converter;

import java.time.LocalDateTime;

/**
 * @author booty
 * @since 2023/10/27
 */
public class String2LocalDateTimeConverter implements Converter<String, LocalDateTime> {


    @Override
    public LocalDateTime convert(String source) {
        if (source == null || source.isEmpty()) return null;
        int length = source.length();
        source = source.replace('T', ' ');
        switch (length) {
            case 10:
                source += " 00:00:00";
                break;
            case 13:
                source += ":00:00";
                break;
            case 16:
                source += ":00";
                break;
            default:
                break;
        }
        return LocalDateTime.parse(source, DateConst.LOCAL_DATE_TIME_FORMATTER);
    }
}
