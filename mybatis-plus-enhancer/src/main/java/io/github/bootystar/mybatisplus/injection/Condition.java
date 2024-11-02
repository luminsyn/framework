package io.github.bootystar.mybatisplus.injection;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 条件
 * @author bootystar
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
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
        throw new InjectException("illegal symbol: " + symbol);
    }
}
