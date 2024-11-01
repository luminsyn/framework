package io.github.bootystar.helper.easyexcel;



import com.alibaba.excel.converters.Converter;
import com.alibaba.excel.converters.DefaultConverterLoader;

import io.github.bootystar.helper.easyexcel.converter.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * EasyExcel转化器工具
 * @author bootystar
 */
public abstract class EasyExcelConverterTool {
    private static final Logger log = LoggerFactory.getLogger(EasyExcelConverterTool.class);
    private static final AtomicBoolean MARK = new AtomicBoolean(false);
    private static final String WRITE_METHOD = "putWriteConverter";
    private static final String ALL_METHOD = "putAllConverter";
    
    private static Method getMethod(String writeMethod) {
        try {
            Class<DefaultConverterLoader> clazz = DefaultConverterLoader.class;
            Method method = clazz.getDeclaredMethod(writeMethod, Converter.class);
            method.setAccessible(true);
            return method;
        }catch (Exception e ){

            log.debug("error",e);
        }
        return null;
    }
    
    private static Method method4Converter2write(){
        return getMethod(WRITE_METHOD);
    }

    private static Method method4Converter2all(){
        return getMethod(ALL_METHOD);
    }

    public static void addConverters(Converter<?>... converters) {
        Method method = method4Converter2write();
        Method method2 = method4Converter2all();
        if (method == null || method2 == null){
            log.warn("EasyExcel add excel converter failed , export or import may produce error on special field!");
            return;
        }
        try {
            for (Converter<?> converter : converters) {
                method.invoke(null, converter);
                method2.invoke(null,converter);
            }
        } catch (IllegalAccessException e) {
            log.warn("IllegalAccessException",e);
        } catch (InvocationTargetException e) {
            log.warn("InvocationTargetException",e);
        }
    }
    
    public static void init() {
        if (MARK.get()){
            return;
        }
        MARK.set(true);
        addConverters( 
                new LocalDateConverter(),
                new LocalDateTimeConverter(),
                new LocalTimeConverter(),
                new TimestampConverter(),
                new TimeConverter(),
                new LongConverter(),
                new DoubleConverter(),
                new BooleanConverter()
        );
    }


    
    



}
