package io.github.bootystar.wechat.officialAccount.entity.event;

import lombok.Data;

/**
 * 扫码推事件的事件推送
 * 件类型，scancode_push
 * 消息类型，event
 * @Author booty
 * @Date 2023/6/13 13:47
 */
@Data
public class ScanCodePush extends BaseEvent {

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
