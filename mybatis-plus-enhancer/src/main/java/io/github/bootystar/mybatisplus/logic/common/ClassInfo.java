package io.github.bootystar.mybatisplus.logic.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author bootystar
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ClassInfo {
    private String classPackage;
    private String classSimpleName;
    private boolean isGenericClass;

}
