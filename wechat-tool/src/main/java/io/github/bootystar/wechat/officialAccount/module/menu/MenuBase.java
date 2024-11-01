package io.github.bootystar.wechat.officialAccount.module.menu;

import lombok.Data;

import java.util.List;

/**
 * 公众号菜单
 * @author bootystar
 * 
 */
@Data
public class MenuBase {
    /**
     * 一级菜单数组，个数应为1~3个
     */
    private List<MenuButton> button;
    /**
     * 二级菜单数组，个数应为1~5个
     * (该属性在查询菜单时存在于一级菜单内配置的button属性中)
     */
    private List<MenuButton> list;


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





}
