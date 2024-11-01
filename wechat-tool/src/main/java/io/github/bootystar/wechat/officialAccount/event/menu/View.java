package io.github.bootystar.wechat.officialAccount.event.menu;

import lombok.Data;

/**
 * 点击菜单跳转链接时的事件推送
 * 消息类型，event
 * 事件类型，VIEW
 * @author bootystar
 * 
 */
@Data
public class View{

    /**
     * 指菜单ID，如果是个性化菜单，则可以通过这个字段，知道是哪个规则的菜单被点击了
     */
    private String MenuID;

    /*
    <xml>
        <ToUserName><![CDATA[toUser]]></ToUserName>
        <FromUserName><![CDATA[FromUser]]></FromUserName>
        <CreateTime>123456789</CreateTime>
        <MsgType><![CDATA[event]]></MsgType>
        <Event><![CDATA[VIEW]]></Event>
        <EventKey><![CDATA[www.qq.com]]></EventKey>
        <MenuId>MENUID</MenuId>
    </xml>
     */
}
