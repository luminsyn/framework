package io.github.bootystar.wechat.miniProgram.enums;

/**
 * @author bootystar
 * 
 */
public enum SuggestEnum {
    /*
    建议，有risky、pass、review三种值
     */
    RISKY("risky"),
    PASS("pass"),
    REVIEW("review"),
    ;

    public final String value;

    SuggestEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
