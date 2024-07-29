package io.github.bootystar.autoconfigure.databind.converter;

import io.github.bootystar.autoconfigure.databind.constant.DateConst;
import org.springframework.core.convert.converter.Converter;

import java.time.LocalDate;

/**
 * @author booty
 * @since 2023/10/27
 */
public class String2LocalDateConverter implements Converter<String, LocalDate> {


    @Override
    public LocalDate convert(String source) {
        if (source.isEmpty()) {
            return null;
        }
        int length = source.length();
        switch (length) {
            case 4:
                source += "-01-01";
                break;
            case 7:
                source += "-01";
                break;
            default:
                break;
        }
        return LocalDate.parse(source, DateConst.LOCAL_DATE_FORMATTER);
    }
}
