package io.github.bootystar.mybatisplus.util;

import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.TypeVariable;

/**
 * @author booty
 */
public abstract class LambdaUtil {

    
    @Data
    @AllArgsConstructor
    public static class LambdaMethod{
        private String classPackage;
        private String classSimpleName;
        private boolean isGenericTypeClass;
        private String methodName;
        private String methodNameFullStr;
        private boolean isStaticMethod;
        private boolean isConstructor;
    }

    /**
     * lambda方法信息
     *
     * @param methodReference lambda方法引用
     * @param parameterClass  参数类型
     * @return {@link LambdaMethod }
     * @author booty
     */
    public static LambdaMethod lambdaMethodInfo(SFunction<?, ?> methodReference,Class<?> parameterClass){
        String methodName = "";
        String fullClassName= "";
        try {
            Method lambdaMethod = methodReference.getClass().getDeclaredMethod("writeReplace");
            lambdaMethod.setAccessible(Boolean.TRUE);
            SerializedLambda serializedLambda  = (SerializedLambda) lambdaMethod.invoke(methodReference);
            fullClassName = serializedLambda.getImplClass().replace("/", ".");
            methodName = serializedLambda.getImplMethodName();
            String methodNameFullStr = methodName;
            Class<?> clazz = Class.forName(fullClassName);
            TypeVariable<? extends Class<?>>[] typeParameters = clazz.getTypeParameters();
            boolean isStaticMethod = false;
            boolean isConstructor = false;
            boolean isGenericTypeClass = typeParameters.length > 0;
            try {
                Method returnMethod = clazz.getMethod(methodNameFullStr, parameterClass);
                Class<?> returnType = returnMethod.getReturnType();
                if (!returnType.equals(clazz)){
                    throw new NoSuchMethodException("no method found return self");
                }
                int modifiers = returnMethod.getModifiers();
                if (Modifier.isStatic(modifiers)){
                    isStaticMethod=true;
                    methodNameFullStr=clazz.getSimpleName()+"."+methodNameFullStr;
                }else{
                    clazz.getConstructor();
                    methodNameFullStr="new "+clazz.getSimpleName()+"()."+methodNameFullStr;
                }
            }catch (NoSuchMethodException e){
                clazz.getConstructor(parameterClass);
                methodNameFullStr="new "+clazz.getSimpleName();
                if (isGenericTypeClass) {
                    methodNameFullStr += "<>";
                }
                isConstructor=true;
            }
            return new LambdaUtil.LambdaMethod(
                    clazz.getPackage().getName()
                    , clazz.getSimpleName()
                    , isGenericTypeClass
                    , methodName
                    , methodNameFullStr
                    , isStaticMethod
                    , isConstructor
            );
        }catch (Exception e){
            String msg= String.format("can't find constructor or method in class [%s] , method name [%s], parameter class [%s]",fullClassName,methodName,parameterClass.getName());
            throw new IllegalStateException(msg);
        }
    }
    
}
