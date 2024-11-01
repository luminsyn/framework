package io.github.bootystar.mybatisplus.injection;

import lombok.Data;

/**
 * @author bootystar
 */
@Data
public class Condition {
    
    private static final String PATTERN = "(?i)(=|>|<|!=|>=|<=|like|not like|is null|is not null|in|not in)";
    
    /**
     * 字段
     */
    private String field;
    
    /**
     * 条件(=,>,<,!=,>=,<=,like,not like,is null,is not null,in,not in)
     * 默认=
     */
    private String symbol;
    
    /**
     * 值
     */
    private Object value;


    public String getSymbol() {
        if (symbol == null || symbol.isEmpty()) return "=";
        if (symbol.matches(PATTERN)) {
            return symbol.toLowerCase();
        }
        throw new AntiInjectException("illegal symbol: " + symbol);
    }
}
