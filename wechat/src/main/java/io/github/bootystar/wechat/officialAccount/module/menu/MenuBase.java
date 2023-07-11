package io.github.bootystar.wechat.officialAccount.module.menu;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.wechat.core.exception.ResponseException;
import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;

import java.util.List;

/**
 * 公众号菜单
 * @Author booty
 * @Date 2023/6/12 9:34
 */
@Data
public class MenuBase {
    /**
     * 一级菜单数组，个数应为1~3个
     */
    private List<MenuButton> button;


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
