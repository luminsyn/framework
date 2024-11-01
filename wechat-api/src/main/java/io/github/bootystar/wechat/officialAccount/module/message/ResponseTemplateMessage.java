package io.github.bootystar.wechat.officialAccount.module.message;

import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;

import java.util.List;

/**
 * 获得模板ID
 * @author bootystar
 * 
 */
@Data
public class ResponseTemplateMessage extends ResponseBase {

    /**
     * 模板列表
     */
    private List<Template> template_list;
    /**
     * 模板在库中对应的id
     */
    private String template_id;

    @Data
    public static class Template {
        /**
         * 模板ID
         */
        private String template_id;
        /**
         * 模板标题
         */
        private String title;
        /**
         * 模板所属行业的一级行业
         */
        private String primary_industry;
        /**
         * 模板所属行业的二级行业
         */
        private String deputy_industry;
        /**
         * 模板内容
         */
        private String content;
        /**
         * 模板示例
         */
        private String example;
    }

}
