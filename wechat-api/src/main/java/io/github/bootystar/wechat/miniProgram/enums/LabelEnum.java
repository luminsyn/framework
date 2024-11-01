package io.github.bootystar.wechat.miniProgram.enums;

/**
 * @author bootystar
 * 
 */
public enum LabelEnum {
    /*
     命中标签枚举值，100 正常；10001 广告；20001 时政；20002 色情；20003 辱骂；20006 违法犯罪；20008 欺诈；20012 低俗；20013 版权；21000 其他
     */
    NORMAL(100,"正常"),
    AD(10001,"广告"),
    POLITICS(20001,"时政政治"),
    PORN(20002,"色情"),
    FUCK(20003,"辱骂"),
    CRIME(20006,"违法犯罪"),
    FOOL(20008,"欺诈"),
    LOW(20012,"低俗"),
    COPYRIGHT(20013,"版权"),
    OTHER(21000,"其他"),

    ;
    public final Integer key;
    public final String value;

    LabelEnum(Integer key, String value) {
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
        for (LabelEnum em : values()) {
            if (em.key.equals(key)){
                return em.value;
            }
        }
        return null;
    }

}
