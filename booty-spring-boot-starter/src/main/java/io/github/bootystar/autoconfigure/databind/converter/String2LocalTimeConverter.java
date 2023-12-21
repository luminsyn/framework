package io.github.bootystar.autoconfigure.databind.converter;

import io.github.bootystar.autoconfigure.databind.constant.DateConst;
import org.springframework.core.convert.converter.Converter;

import java.time.LocalTime;

/**
 * @author booty
 * @since 2023/10/27
 */
public class String2LocalTimeConverter implements Converter<String, LocalTime> {


    @Override
    public LocalTime convert(String source) {
        if (source.isEmpty()) {
            return null;
        }
        int length = source.length();
        switch (length) {
            case 2:
                source += ":00:00";
                break;
            case 5:
                source += ":00";
            default:
                break;
        }
        return LocalTime.parse(source, DateConst.LOCAL_TIME_FORMATTER);
    }

}
