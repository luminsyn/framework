package io.github.bootystar.wechat.officialAccount.event.menu;

import lombok.Data;

/**
 * 点击菜单拉取消息时的事件推送
 * 消息类型，event
 * 事件类型，CLICK
 * @author bootystar
 * 
 */
@Data
public class Click {


    /*
    推送XML数据包示例
    <xml>
        <ToUserName><![CDATA[toUser]]></ToUserName>
        <FromUserName><![CDATA[FromUser]]></FromUserName>
        <CreateTime>123456789</CreateTime>
        <MsgType><![CDATA[event]]></MsgType>
        <Event><![CDATA[CLICK]]></Event>
        <EventKey><![CDATA[EVENTKEY]]></EventKey>
    </xml>
     */


}
