package io.github.bootystar.wechat.miniProgram.enums;


/**
 * @author bootystar
 * 
 */

public enum SceneEnum {
    /*
    场景枚举值（1 资料；2 评论；3 论坛；4 社交日志）
     */
    DATA(1,"资料"),
    COMMENT(2,"评论"),
    FORUM(3,"论坛"),
    SOCIAL(4,"社交日志"),
    ;
    public final Integer key;
    public final String value;

    SceneEnum(Integer key, String value) {
        this.key = key;
        this.value = value;
    }

    public Integer getKey() {
        return key;
    }

    public String getValue() {
        return value;
    }

    public static String getValueByKey(Integer key){
        for (SceneEnum em : values()) {
            if (em.key.equals(key)){
                return em.value;
            }
        }
        return null;
    }
}
