package io.github.bootystar.wechat.officialAccount.module.menu;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.wechat.core.ResponseBase;
import io.github.bootystar.wechat.core.exception.ResponseException;

/**
 * @author booty
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
     * @author booty
     *
     */
    public static ResponseBase createMenu(String accessToken, MenuBase menuCreate){
        String json = JSON.toJSONString(menuCreate);
        String url = POST_JSON_CREATE_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doPostJson(url, json);
        ResponseBase responseBase = JSON.parseObject(result, ResponseBase.class);
        if (responseBase.getErrcode()!=null && responseBase.getErrcode()!=0){
            throw new ResponseException(responseBase);
        }
        return responseBase;
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
     * @author booty
     *
     */
    public static ResponseMenuQuery queryMenu(String accessToken){
        String url = GET_QUERY_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doGet(url);
        ResponseMenuQuery responseBase = JSON.parseObject(result, ResponseMenuQuery.class);
        if (responseBase.getErrcode()!=null && responseBase.getErrcode()!=0){
            throw new ResponseException(responseBase);
        }
        return responseBase;
    }

    /**
     * 删除菜单
     */
    private static final String GET_DELETE_MENU_URL="https://api.weixin.qq.com/cgi-bin/menu/delete?access_token=ACCESS_TOKEN";

    public static ResponseBase deleteMenu(String accessToken){
        String url = GET_DELETE_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doGet(url);
        ResponseMenuQuery responseBase = JSON.parseObject(result, ResponseMenuQuery.class);
        if (responseBase.getErrcode()!=null && responseBase.getErrcode()!=0){
            throw new ResponseException(responseBase);
        }
        return responseBase;
    }



}
