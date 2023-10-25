package io.github.bootystar.wechat.officialAccount.module.message;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.wechat.core.ResponseBase;
import io.github.bootystar.wechat.core.exception.ResponseException;
import lombok.Data;

/**
 *
 * 设置所属行业
 * @author booty
 * @since 2023/6/16 16:59
 */
@Data
public class ResponseIndustry extends ResponseBase {
    /**
     * 帐号设置的主营行业
     */
    private IndustryClass primary_industry;
    /**
     * 帐号设置的副营行业
     */
    private IndustryClass secondary_industry;



    /**
     * 行业class
     */
    @Data
    public static class IndustryClass {
        private String first_class;
        private String second_class;
    }

}
