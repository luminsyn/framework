package io.github.bootystar.wechat.officialAccount.event.menu;

import lombok.Data;

/**
 * 扫码推事件的事件推送
 * 件类型，scancode_push
 * 消息类型，event
 * @author bootystar
 * 
 */
@Data
public class ScanCodePush {

    /*
    <xml>
        <ToUserName><![CDATA[gh_e136c6e50636]]></ToUserName>
        <FromUserName><![CDATA[oMgHVjngRipVsoxg6TuX3vz6glDg]]></FromUserName>
        <CreateTime>1408090502</CreateTime>
        <MsgType><![CDATA[event]]></MsgType>
        <Event><![CDATA[scancode_push]]></Event>
        <EventKey><![CDATA[6]]></EventKey>
        <ScanCodeInfo>
            <ScanType><![CDATA[qrcode]]></ScanType>
            <ScanResult><![CDATA[1]]></ScanResult>
        </ScanCodeInfo>
    </xml>
     */

}
