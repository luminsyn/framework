package io.github.bootystar.wechat.officialAccount.entity.menu;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.wechat.common.exception.WechatResponseException;
import io.github.bootystar.wechat.entity.ResponseBase;
import lombok.Data;

import java.util.List;

/**
 * 公众号菜单
 * @Author booty
 * @Date 2023/6/12 9:34
 */
@Data
public class Menu {
    /*
    {
         "button":[
         {
              "type":"click",
              "name":"今日歌曲",
              "key":"V1001_TODAY_MUSIC"
          },
          {
               "name":"菜单",
               "sub_button":[
               {
                   "type":"view",
                   "name":"搜索",
                   "url":"http://www.soso.com/"
                },
                {
                     "type":"miniprogram",
                     "name":"wxa",
                     "url":"http://mp.weixin.qq.com",
                     "appid":"wx286b93c14bbf93aa",
                     "pagepath":"pages/lunar/index"
                 },
                {
                   "type":"click",
                   "name":"赞一下我们",
                   "key":"V1001_GOOD"
                }]
           }]
     }
     */
    /**
     * 一级菜单数组，个数应为1~3个
     */
    private List<MenuButton> button;


    /**
     * 创建接口链接
     */
    private static final String POST_JSON_CREATE_MENU_URL="https://api.weixin.qq.com/cgi-bin/menu/create?access_token=ACCESS_TOKEN";
    /**
     * 创建菜单
     *
     * @param menuCreate        菜单
     * @param accessToken 访问令牌
     * @return {@code ResponseBase }
     * @author booty
     * @date 2023/06/12 11:49
     */
    public static ResponseBase createMenu(String accessToken, Menu menuCreate){
        String json = JSON.toJSONString(menuCreate);
        String url = POST_JSON_CREATE_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doPostJson(url, json);
        ResponseBase responseBase = JSON.parseObject(result, ResponseBase.class);
        if (responseBase.getErrcode()!=0){
            throw new WechatResponseException(responseBase);
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
     * @date 2023/06/13 10:42
     */
    public static MenuQuery queryMenu(String accessToken){
        String url = GET_QUERY_MENU_URL.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doGet(url);
        MenuQuery responseBase = JSON.parseObject(result, MenuQuery.class);
        if (responseBase.getErrcode()!=0){
            throw new WechatResponseException(responseBase);
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
        MenuQuery responseBase = JSON.parseObject(result, MenuQuery.class);
        if (responseBase.getErrcode()!=0){
            throw new WechatResponseException(responseBase);
        }
        return responseBase;
    }


}
