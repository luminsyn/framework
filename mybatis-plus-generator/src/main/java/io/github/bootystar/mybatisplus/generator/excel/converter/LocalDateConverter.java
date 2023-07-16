package io.github.bootystar.mybatisplus.generator.excel.converter;

import com.alibaba.excel.converters.Converter;
import com.alibaba.excel.enums.CellDataTypeEnum;
import com.alibaba.excel.metadata.GlobalConfiguration;
import com.alibaba.excel.metadata.data.ReadCellData;
import com.alibaba.excel.metadata.data.WriteCellData;
import com.alibaba.excel.metadata.property.ExcelContentProperty;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * @Author booty
 * @Date 2023/6/30 9:25
 */
public class LocalDateConverter implements Converter<LocalDate> {
    private final DateTimeFormatter formatter =DateTimeFormatter.ofPattern("yyyy-MM-dd");

    @Override
    public Class<LocalDate> supportJavaTypeKey() {
        return LocalDate.class;
    }
    @Override
    public CellDataTypeEnum supportExcelTypeKey() {
        return CellDataTypeEnum.STRING;
    }
    @Override
    public LocalDate convertToJavaData(ReadCellData cellData, ExcelContentProperty contentProperty,
                                       GlobalConfiguration globalConfiguration) {
        return LocalDate.parse(cellData.getStringValue(), formatter);
    }
    @Override
    public WriteCellData<String> convertToExcelData(LocalDate value, ExcelContentProperty contentProperty,
                                                    GlobalConfiguration globalConfiguration) {
        return new WriteCellData<>(value.format(formatter));
    }
}
