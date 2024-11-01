package io.github.bootystar.wechat.miniProgram.module.msg;

import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;

/**
 * @author bootystar
 * @Date 2023/6/25 10:52
 */
@Data
public class MediaCheckResponse extends ResponseBase {

    /**
     * 唯一请求标识，标记单次请求，用于匹配异步推送结果
     */
    private String trace_id;
}
