package io.github.bootystar.mybatisplus.logic.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author bootystar
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class LambdaMethod {
    private String classPackage;
    private String classSimpleName;
    private boolean isGenericClass;
    private String methodName;
    private boolean isStaticMethod;
    private boolean isConstructor;
    private boolean isGenericMethod;

    public String invokeStr(String parameter, String returnType) {
        if (methodName == null) {
            return parameter;
        }
        if (isStaticMethod) {
            return String.format("%s.%s(" , classSimpleName, methodName);
        }
        if (isConstructor) {
            return String.format("new %s%s(%s)" , classSimpleName, isGenericClass ? "<>" : "" , parameter);
        }
        if (isGenericMethod) {
            return String.format("new %s<>().%s(%s)" , classSimpleName, methodName, parameter);
        }
        if (isGenericClass) {
            return String.format("new %s<%s>().%s(%s)" , classSimpleName, returnType, methodName, parameter);
        }
        return String.format("new %s().%s(%s)" , classSimpleName, methodName, parameter);
    }

    public String returnStr(String returnType) {
        if (classSimpleName == null) {
            return "";
        }
        if (isGenericClass) {
            return String.format("%s<%s>" , classSimpleName, returnType);
        }
        return String.format("%s" , classSimpleName);
    }
}
