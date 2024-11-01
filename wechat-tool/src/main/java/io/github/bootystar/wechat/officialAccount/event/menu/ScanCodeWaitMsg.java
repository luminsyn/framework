package io.github.bootystar.wechat.officialAccount.event.menu;

import lombok.Data;

/**
 * 扫码推事件且弹出“消息接收中”提示框的事件推送
 * 消息类型，event
 * 事件类型，scancode_waitmsg
 * @author bootystar
 * 
 */
@Data
public class ScanCodeWaitMsg {

    /**
     * 扫描信息
     */
    private String ScanCodeInfo;
    /**
     * 扫描类型，一般是qrcode
     */
    private String ScanType;
    /**
     * 扫描结果，即二维码对应的字符串信息
     */
    private String ScanResult;
    /*
    <xml>
        <ToUserName><![CDATA[gh_e136c6e50636]]></ToUserName>
        <FromUserName><![CDATA[oMgHVjngRipVsoxg6TuX3vz6glDg]]></FromUserName>
        <CreateTime>1408090606</CreateTime>
        <MsgType><![CDATA[event]]></MsgType>
        <Event><![CDATA[scancode_waitmsg]]></Event>
        <EventKey><![CDATA[6]]></EventKey>
        <ScanCodeInfo>
            <ScanType><![CDATA[qrcode]]></ScanType>
            <ScanResult><![CDATA[2]]></ScanResult>
        </ScanCodeInfo>
    </xml>
     */
}
