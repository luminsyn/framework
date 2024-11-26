package io.github.bootystar.mybatisplus.logic.info;

import lombok.Getter;


/**
 * @author bootystar
 */
@Getter
public class ClassInfo {
    protected String classPackage;
    protected String classSimpleName;
    protected int classGenericTypeCount;

    public ClassInfo(Class<?> clazz) {
        this.classPackage = clazz.getPackage().getName();
        this.classSimpleName = clazz.getSimpleName();
        this.classGenericTypeCount = clazz.getTypeParameters().length;
    }

    public ClassInfo(String classPackage, String classSimpleName, int classGenericTypeCount) {
        this.classPackage = classPackage;
        this.classSimpleName = classSimpleName;
        this.classGenericTypeCount = classGenericTypeCount;
    }

    public String classDeclaration(String genericTypeStr) {
        if (classSimpleName == null) {
            return genericTypeStr;
        }
        if (classGenericTypeCount == 1) {
            return String.format("%s<%s>" , classSimpleName, genericTypeStr);
        }
        return classSimpleName;
    }

}
