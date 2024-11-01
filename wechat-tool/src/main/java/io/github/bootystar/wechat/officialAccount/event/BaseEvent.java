package io.github.bootystar.wechat.officialAccount.event;

/**
 * 事件推送基类
 * @author bootystar
 * 
 */
public abstract class BaseEvent {
    /**
     * 开发者 微信号
     */
    private String ToUserName;
    /**
     * 发送方帐号（一个OpenID）
     */
    private String FromUserName;
    /**
     * 消息创建时间
     */
    private String CreateTime;
    /**
     * 消息类型
     */
    private String MsgType;
    /**
     * 事事件类型
     */
    private String Event;
    /**
     * 事件KEY值，与自定义菜单接口中KEY值对应
     *
     */
    private String EventKey;

    /*
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
