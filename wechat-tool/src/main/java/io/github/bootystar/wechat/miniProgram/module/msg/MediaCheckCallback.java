package io.github.bootystar.wechat.miniProgram.module.msg;

import io.github.bootystar.wechat.core.ResponseBase;
import io.github.bootystar.wechat.miniProgram.enums.SuggestEnum;
import lombok.Data;

import java.util.List;

/**
 * @author bootystar
 * @Date 2023/6/15 11:02
 */
@Data
public class MediaCheckCallback extends ResponseBase {

    private String ToUserName;
    private String FromUserName;
    private Long CreateTime;
    private String MsgType;
    private String Event;
    private String appid;
    private String trace_id;
    private String version;


    /**
     * 详细检测结果
     */
    private List<MsgSecCheckResponseDetail> detail;

    /**
     * 综合结果
     */
    private MsgSecCheckResponseResult result;

    public boolean passed(){
        return SuggestEnum.PASS.value.equals(result.getSuggest());
    }



}
