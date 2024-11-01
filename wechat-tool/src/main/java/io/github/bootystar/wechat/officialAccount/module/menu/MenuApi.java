package io.github.bootystar.wechat.officialAccount.module.menu;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.wechat.tool.HttpTool;
import io.github.bootystar.wechat.core.ResponseBase;

/**
 * @author bootystar
 * 
 */
public class MenuApi {


    private MenuApi() {
    }



    /**
     * 创建接口链接
     */
    private static final String POST_JSON_CREATE_MENU_URL="https://api.weixin.qq.com/cgi-bin/menu/create?access_token=ACCESS_TOKEN";
    /**
     * 创建菜单
     *
     * @param menuCreate  菜单
     * @param accessToken 访问令牌
     * @return {@code ResponseBase }
     * @author bootystar
     * 
     */
    public static ResponseBase createMenu(String accessToken, MenuBase menuCreate){
        String json = JSON.toJSONString(menuCreate);
        String url = POST_JSON_CREATE_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doPostJson(url, json);
        ResponseBase response = JSON.parseObject(result, ResponseBase.class);
        response.check();
        return response;
    }

    /**
     * 查询接口链接
     */
    private static final String GET_QUERY_MENU_URL="https://api.weixin.qq.com/cgi-bin/get_current_selfmenu_info?access_token=ACCESS_TOKEN";


    /**
     * 查询菜单
     *
     * @param accessToken 访问令牌
     * @return {@code ResponseBase }
     * @author bootystar
     * 
     */
    public static ResponseMenuQuery queryMenu(String accessToken){
        String url = GET_QUERY_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doGet(url);
        ResponseMenuQuery response = JSON.parseObject(result, ResponseMenuQuery.class);
        response.check();
        return response;
    }

    /**
     * 删除菜单
     */
    private static final String GET_DELETE_MENU_URL="https://api.weixin.qq.com/cgi-bin/menu/delete?access_token=ACCESS_TOKEN";

    public static ResponseBase deleteMenu(String accessToken){
        String url = GET_DELETE_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doGet(url);
        ResponseMenuQuery response = JSON.parseObject(result, ResponseMenuQuery.class);
        response.check();
        return response;
    }



}
