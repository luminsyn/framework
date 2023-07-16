package io.github.bootystar.mybatisplus.generator.excel.converter;

import com.alibaba.excel.converters.Converter;
import com.alibaba.excel.enums.CellDataTypeEnum;
import com.alibaba.excel.metadata.GlobalConfiguration;
import com.alibaba.excel.metadata.data.ReadCellData;
import com.alibaba.excel.metadata.data.WriteCellData;
import com.alibaba.excel.metadata.property.ExcelContentProperty;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * @Author booty
 * @Date 2023/6/30 9:25
 */
public class LocalTimeConverter implements Converter<LocalTime> {
    private final DateTimeFormatter formatter =DateTimeFormatter.ofPattern("HH:mm:ss");

    @Override
    public Class<LocalTime> supportJavaTypeKey() {
        return LocalTime.class;
    }
    @Override
    public CellDataTypeEnum supportExcelTypeKey() {
        return CellDataTypeEnum.STRING;
    }
    @Override
    public LocalTime convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty,
                                       GlobalConfiguration globalConfiguration) {
        return LocalTime.parse(cellData.getStringValue(), formatter);
    }
    @Override
    public WriteCellData<String> convertToExcelData(LocalTime value, ExcelContentProperty contentProperty,
                                                    GlobalConfiguration globalConfiguration) {
        return new WriteCellData<>(value.format(formatter));
    }
}
