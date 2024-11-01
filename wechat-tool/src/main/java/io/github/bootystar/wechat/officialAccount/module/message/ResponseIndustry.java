package io.github.bootystar.wechat.officialAccount.module.message;

import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * 设置所属行业
 * @author bootystar
 * 
 */

@Data
@EqualsAndHashCode(callSuper = true)
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
