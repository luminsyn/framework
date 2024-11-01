package io.github.bootystar.wechat.miniProgram.module.msg;


import io.github.bootystar.wechat.core.ResponseBase;
import io.github.bootystar.wechat.miniProgram.enums.LabelEnum;
import io.github.bootystar.wechat.miniProgram.enums.SuggestEnum;
import lombok.Data;

import java.util.List;

/**
 * @author bootystar
 * 
 */
@Data
public class MsgSecCheckResponse extends ResponseBase {
    /**
     * 详细检测结果
     */
    private List<MsgSecCheckResponseDetail> detail;
    /**
     * 唯一请求标识，标记单次请求
     */
    private String 	trace_id;
    /**
     * 综合结果
     */
    private MsgSecCheckResponseResult result;

    public boolean passed(){
        return SuggestEnum.PASS.value.equals(result.getSuggest());
    }


    public String generateInformation(){
        if (passed()){
            return "风险检测通过";
        }
        StringBuilder sb=new StringBuilder();
        sb.append("风险检测未通过,检测涉及到以下敏感内容：【");
        sb.append(LabelEnum.getValueByKey(result.getLabel()));
        sb.append("】");
        return sb.toString();
    }

}
