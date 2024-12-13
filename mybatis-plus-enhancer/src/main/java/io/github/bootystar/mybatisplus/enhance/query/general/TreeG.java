package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * 条件树
 *
 * @author bootystar
 */
@Setter
@Getter
public class TreeG implements ISqlTree {

    /**
     * 查询条件列表
     */
    @ApiModelProperty("条件列表")
    @Schema(description = "条件列表")
    protected LinkedHashSet<ConditionG> conditions;

    /**
     * 子条件
     * (满足父条件后的值才会筛选子条件)
     */
    @ApiModelProperty("子条件")
    @Schema(description = "子条件")
    protected TreeG child;

    /**
     * 排序条件列表
     */
    @ApiModelProperty("排序条件(仅最上层有效)")
    protected LinkedHashSet<SortG> sorts;

}
