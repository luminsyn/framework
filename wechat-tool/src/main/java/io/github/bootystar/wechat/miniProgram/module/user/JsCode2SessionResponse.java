package io.github.bootystar.wechat.miniProgram.module.user;

import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author bootystar
 * @since 2023/12/29
 */
@NoArgsConstructor
@Data
public class JsCode2SessionResponse extends ResponseBase {


    /**
     * openid
     */
    private String openid;
    /**
     * session_key
     */
    private String session_key;
    /**
     * unionid
     */
    private String unionid;
}
