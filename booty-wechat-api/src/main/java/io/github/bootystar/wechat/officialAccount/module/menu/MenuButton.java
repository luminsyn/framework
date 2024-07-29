package io.github.bootystar.wechat.officialAccount.module.menu;

import io.github.bootystar.wechat.officialAccount.enums.MenuButtonTypeEnum;
import lombok.Data;

import java.util.List;

/**
 * 每个菜单的小按钮
 * @author booty
 * 
 */
@Data
public class MenuButton {
    /**
     * 菜单的响应动作类型，view表示网页类型，click表示点击类型，miniprogram表示小程序类型
     */
    private String type;
    /**
     * 菜单标题，不超过16个字节，子菜单不超过60个字节
     */
    private String name;
    /**
     * click等点击类型必须
     * 菜单KEY值，用于消息接口推送，不超过128字节
     */
    private String key;
    /**
     * view、miniprogram类型必须
     * 网页 链接，用户点击菜单可打开链接，不超过1024字节。 type为miniprogram时，不支持小程序的老版本客户端将打开本url。
     */
    private String url;

    /**
     * 对于不同的菜单类型，value的值意义不同。官网上设置的自定义菜单： Text:保存文字到value； Img、voice：保存mediaID到value； Video：保存视频下载链接到value； News：保存图文消息到news_info，同时保存mediaID到value； View：保存链接到url。 使用API设置的自定义菜单： click、scancode_push、scancode_waitmsg、pic_sysphoto、pic_photo_or_album、 pic_weixin、location_select：保存值到key；view：保存链接到url
     */
    private String value;
    /**
     * 图文消息的信息(仅官网创建，不支持api)
     */
    private NewsInfo news_info;

    /**
     * media_id类型和view_limited类型必须
     * 调用新增永久素材接口返回的合法media_id
     */
    private String media_id;
    /**
     * miniprogram类型必须
     * 小程序的appid（仅认证公众号可配置）
     */
    private String appid;
    /**
     * miniprogram类型必须
     * 小程序的页面路径
     */
    private String pagepath;
    /**
     * article_id类型和article_view_limited类型必须
     * 发布后获得的合法 article_id
     */
    private String article_id;
    /**
     * 二级菜单数组，个数应为1~5个
     */
    private List<MenuBase> sub_button;

    /**
     * 图文信息的菜单（仅官网创建）
     */
    @Data
    public static class NewsInfo{
        /**
         * 图文消息的信息
         */
        private String title;
        /**
         * 作者
         */
        private String author;
        /**
         * 摘要
         */
        private String digest;
        /**
         * 是否显示封面，0为不显示，1为显示
         */
        private Boolean show_cover;
        /**
         * 封面图片的URL
         */
        private String cover_url;
        /**
         * 正文的URL
         */
        private String content_url;
        /**
         * 原文的URL，若置空则无查看原文入口
         */
        private String source_url;
    }


    /**
     * 创建按钮实例
     *
     * @param type 类型
     * @param name 名字
     * @return {@code MenuButton }
     * @author booty
     * 
     */
    public static MenuButton createButtonInstance(MenuButtonTypeEnum type, String name){
        MenuButton button = new MenuButton();
        button.setType(type.value);
        button.setName(name);
        return button;
    }

}
