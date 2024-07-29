package io.github.bootystar.autoconfigure.prop;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "booty.easy-excel")
@Data
public class EasyExcelProp {

    private Boolean enhancedConverter = true;


}
