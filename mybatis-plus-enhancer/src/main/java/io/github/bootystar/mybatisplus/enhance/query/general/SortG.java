package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 排序参数
 *
 * @author bootystar
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SortG implements ISqlSort {

    /**
     * 字段/属性名
     */
    @ApiModelProperty("属性名")
    @Schema(description = "属性名")
    protected String field;

    /**
     * (默认否, 为否时无需填写)
     */
    @ApiModelProperty("是否倒序(默认否, 为否时无需填写)")
    @Schema(description = "否倒序(默认否, 为否时无需填写)")
    protected boolean desc;

    public static SortG of(ISqlSort sort) {
        if (sort instanceof SortG) return (SortG) sort;
        return new SortG(sort.getField(), sort.isDesc());
    }

}
