package io.github.bootystar.wechat.officialAccount.enums;

/**
 * api的请求地址查询枚举
 * @author bootystar
 * 
 */
public enum CgiPathEnum {

    GET_ACCESS_TOKEN("获取accessToken","/cgi-bin/token"),
    SET_INDUSTRY("设置所属行业","/cgi-bin/template/api_set_industry"),
    KF_ACCOUNT_ADD("添加客服帐号","/customservice/kfaccount/add"),
    KF_ACCOUNT_UPDATE("修改客服帐号","/customservice/kfaccount/update"),
    KF_ACCOUNT_DELETE("删除客服帐号","/customservice/kfaccount/del"),
    KF_ACCOUNT_UPDATE_HEAD_IMG("删除客服帐号","/customservice/kfaccount/uploadheadimg"),

    ;
    public final String name;
    public final String url;

    CgiPathEnum(String name, String url) {
        this.name = name;
        this.url = url;
    }

    public String getName() {
        return name;
    }

    public String getUrl() {
        return url;
    }
}
