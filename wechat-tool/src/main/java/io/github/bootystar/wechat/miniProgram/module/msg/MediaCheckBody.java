package io.github.bootystar.wechat.miniProgram.module.msg;

import lombok.Data;

/**
 * @author bootystar
 * @Date 2023/6/15 11:02
 */
@Data
public class MediaCheckBody {

    /**
     * @required
     * 需检测的文本内容，文本字数的上限为2500字，需使用UTF-8编码
     */
    private String media_url;
    /**
     * @required
     * 1:音频;2:图片
     */
    private Integer media_type;

    /**
     * @required
     * 接口版本号，2.0版本为固定值2
     */
    private Integer version=2;
    /**
     * @required
     * 场景枚举值（1 资料；2 评论；3 论坛；4 社交日志）
     */
    private Integer scene;
    /**
     * @required
     * 用户的openid（用户需在近两小时访问过小程序）
     */
    private String openid;


}
