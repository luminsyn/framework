package io.github.bootystar.wechat.officialAccount.event.message;

/**
 * 在模版消息发送任务完成后，微信服务器会将是否送达成功作为通知
 * 送达由于用户拒收（用户设置拒绝接收公众号消息）而失败时，推送的XML
 * @author bootystar
 * 
 */
public class MsgSendRejected {
    /*
        <xml>
          <ToUserName><![CDATA[gh_7f083739789a]]></ToUserName>
          <FromUserName><![CDATA[oia2TjuEGTNoeX76QEjQNrcURxG8]]></FromUserName>
          <CreateTime>1395658984</CreateTime>
          <MsgType><![CDATA[event]]></MsgType>
          <Event><![CDATA[TEMPLATESENDJOBFINISH]]></Event>
          <MsgID>200163840</MsgID>
          <Status><![CDATA[failed:user block]]></Status>
        </xml>
     */


}
