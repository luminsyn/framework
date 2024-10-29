package io.github.bootystar.wechat.officialAccount.module.message;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.wechat.tool.HttpTool;
import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 消息发送api
 * @author booty
 * 
 */
public class MessageApi {

    private MessageApi() {
    }


    private static final String POST_JSON_SET_INDUSTRY ="https://api.weixin.qq.com/cgi-bin/template/api_set_industry?access_token=ACCESS_TOKEN";

    /**
     * 设置所属行业
     *
     * @param accessToken 访问令牌
     * @param id1         主行业id
     * @param id2         副行业id
     * @return {@code ResponseBase }
     * @author booty
     * 
     */
    public static ResponseBase setIndustry(String accessToken, Integer id1,Integer id2){
        String url = POST_JSON_SET_INDUSTRY.replace("ACCESS_TOKEN", accessToken);
        JSONObject json = new JSONObject();
        json.put("industry_id1",id1);
        json.put("industry_id2",id2);
        String result = HttpTool.doPostJson(url,json.toJSONString());
        ResponseBase response = JSON.parseObject(result, ResponseBase.class);
        response.check();
        return response;
    }


    private static final String GET_QUERY_INDUSTRY ="https://api.weixin.qq.com/cgi-bin/template/get_industry?access_token=ACCESS_TOKEN";

    /**
     * 获取设置的行业信息
     *
     * @param accessToken 访问令牌
     * @return {@code Industry }
     * @author booty
     * 
     */
    public static ResponseIndustry getIndustry(String accessToken){
        String url = GET_QUERY_INDUSTRY.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doGet(url);
        /*
            {
                "primary_industry":{"first_class":"运输与仓储","second_class":"快递"},
                "secondary_industry":{"first_class":"IT科技","second_class":"互联网|电子商务"}
            }
        */

        ResponseIndustry response = JSON.parseObject(result, ResponseIndustry.class);
        response.check();
        return response;
    }









    /**
     * 获得模板ID
     */
    private static final String POST_JSON_ADD_TEMPLATE ="https://api.weixin.qq.com/cgi-bin/template/api_add_template?access_token=ACCESS_TOKEN";

    /**
     * 获得模板ID
     * 从行业模板库选择模板到账号后台，获得模板ID的过程可在微信公众平台后台完成
     * 该方法仅返回id字段
     *
     * @param accessToken 访问令牌
     * @param shortId     模板库中模板的编号，有“TM**”和“OPENTMTM**”等形式,对于类目模板，为纯数字ID
     * @param keywords    选用的类目模板的关键词,按顺序传入,如果为空，或者关键词不在模板库中，会返回40246错误码
     * @return {@code ResponseBase }
     * @author booty
     */
    public static ResponseTemplateMessage addTemplate(String accessToken, String shortId,String... keywords){
        String url = POST_JSON_ADD_TEMPLATE.replace("ACCESS_TOKEN", accessToken);
        JSONObject json = new JSONObject();
        json.put("template_id_short",shortId);
        json.put("keyword_name_list",keywords);
        String result = HttpTool.doPostJson(url,json.toJSONString());
        ResponseTemplateMessage response = JSON.parseObject(result, ResponseTemplateMessage.class);
        response.check();
        return response;
    }


    private static final String GET_DELETE_TEMPLATE ="https://api.weixin.qq.com/cgi-bin/template/get_all_private_template?access_token=ACCESS_TOKEN";

    /**
     * 得到所有模板列表
     * 该方法仅返回template_list字段
     *
     * @param accessToken 访问令牌
     * @return {@code Templates }
     * @author booty
     * 
     */
    public static ResponseTemplateMessage getAllPrivateTemplate(String accessToken){
        String url = GET_DELETE_TEMPLATE.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doGet(url);
        ResponseTemplateMessage response = JSON.parseObject(result, ResponseTemplateMessage.class);
        response.check();
        return response;
    }


    private static final String POST_JSON_DELETE_TEMPLATE ="https://api.weixin.qq.com/cgi-bin/template/del_private_template?access_token=ACCESS_TOKEN";


    /**
     * 删除模板
     *
     * @param accessToken 访问令牌
     * @param templateId  模板id
     * @return {@code ResponseBase }
     * @author booty
     * 
     */
    public static ResponseBase deleteTemplate(String accessToken, String templateId){
        String url = POST_JSON_DELETE_TEMPLATE.replace("ACCESS_TOKEN", accessToken);
        JSONObject json = new JSONObject();
        json.put("template_id",templateId);
        String result = HttpTool.doPostJson(url,json.toJSONString());
        ResponseBase response = JSON.parseObject(result, ResponseBase.class);
        response.check();
        return response;
    }




    /**
     * 获取消息发送体
     * （不适用与类目模板消息）
     * 默认json格式：
     *      {
     *            "touser":"OPENID",
     *            "template_id":"ngqIpbwh8bUfcSsECmogfXcV14J0tQlEpBO27izEYtY",
     *            "url":"http://weixin.qq.com/download",
     *            "miniprogram":{
     *              "appid":"xiaochengxuappid12345",
     *              "pagepath":"index?foo=bar"
     *            },
     *            "client_msg_id":"MSG_000001",
     *            "data":{
     *
     *                    "keyword1":{
     *                        "value":"巧克力"
     *                    },
     *                    "keyword2": {
     *                        "value":"39.8元"
     *                    },
     *                    "keyword3": {
     *                        "value":"2014年9月22日"
     *                    }
     *            }
     *        }
     *
     *
     *
     * @param openId              开放id
     * @param templateId          模板id
     * @param url                 跳转url
     * @param miniProgramOpenId   迷你程序开放id
     * @param miniProgramPagePath 迷你程序页面路径
     * @param clientMsgId         防重入id
     * @param keywords            关键字
     * @return {@code Map<?,?> }
     * @author booty
     * 
     */
    public static Map<?,?> getSendBody(String openId, String templateId, String url, String miniProgramOpenId, String miniProgramPagePath, String clientMsgId, String... keywords){

        Map<String, Object> sendBody = new HashMap<>();
        sendBody.put("touser", openId);
        sendBody.put("template_id", templateId);

        // url
        if (url!=null && !url.equals("")){
            sendBody.put("url", url);
        }

        // 小程序
        if (miniProgramOpenId!=null && !miniProgramOpenId.equals("")){
            HashMap<String, String> miniprogram = new HashMap<>();
            miniprogram.put("appid",miniProgramOpenId);
            if (miniProgramPagePath!=null && !miniProgramPagePath.equals("")){
                miniprogram.put("pagepath",miniProgramPagePath);
            }
            sendBody.put("miniprogram",miniprogram);
        }

        // 防重入id
        if (clientMsgId!=null && !clientMsgId.equals("")){
            sendBody.put("client_msg_id", clientMsgId);
        }

        // 数据
        Map<String, Object> sendData = new HashMap<>();
        String key="keyword";
        for (int i = 0; i < keywords.length; i++) {
            int i1 = i+1;
            sendData.put(key+i1, new KeyWord(keywords[i]));
        }
        sendBody.put("data", sendData);

        return sendBody;
    }


    /**
     * 获取消息发送体
     * （兼容类目模板消息）
     *
     * 类目模板json
     * {
     *   "touser": "OPENID",
     *   "template_id": "TEMPLATE_ID",
     *   "page": "index",
     *   "data": {
     *       "thing01": {
     *           "value": "某某"
     *       },
     *       "amount01": {
     *           "value": "￥100"
     *       },
     *       "thing02": {
     *           "value": "广州至北京"
     *       },
     *       "time01": {
     *           "value": "2019年10月1日 15:00"
     *       }
     *   }
     * }
     *
     * @param openId              开放id
     * @param templateId          模板id
     * @param url                 url
     * @param miniProgramOpenId   迷你程序开放id
     * @param miniProgramPagePath 迷你程序页面路径
     * @param clientMsgId         客户机味精id
     * @param keywordMap          防重入id
     * @return {@code Map<?,?> }
     * @author booty
     * 
     */
    public static Map<?,?> getSendBody(String openId,String templateId, String url,String miniProgramOpenId,String miniProgramPagePath, String clientMsgId,  Map<?, ?> keywordMap){
        Map<String, Object> sendBody = new HashMap<>();
        sendBody.put("touser", openId);
        sendBody.put("template_id", templateId);
        // url
        if (url!=null && !url.equals("")){
            sendBody.put("url", url);
        }

        // 小程序
        if (miniProgramOpenId!=null && !miniProgramOpenId.equals("")){
            HashMap<String, String> miniprogram = new HashMap<>();
            miniprogram.put("appid",miniProgramOpenId);
            if (miniProgramPagePath!=null && !miniProgramPagePath.equals("")){
                miniprogram.put("pagepath",miniProgramPagePath);
            }
            sendBody.put("miniprogram",miniprogram);
        }

        // 防重入id
        if (clientMsgId!=null && !clientMsgId.equals("")){
            sendBody.put("client_msg_id", clientMsgId);
        }

        // 数据
        sendBody.put("data", keywordMap);
        return sendBody;
    }


    /**
     * 构造发送体data数据的key：value格式的map
     *
     * @param sourceMap 源图
     * @return {@code Map<?,?> }
     * @author booty
     * 
     */
    public static Map<?,?> generatorDataMap(Map<?, ?> sourceMap){
        LinkedHashMap<String, Object> map = new LinkedHashMap<>();
        Iterator<? extends Map.Entry<?, ?>> iterator = sourceMap.entrySet().iterator();
        while (iterator.hasNext()){
            Map.Entry<?, ?> next = iterator.next();
            map.put(next.getKey().toString(),new KeyWord(next.getValue().toString()));
        }
        return map;
    }



    /**
     * 模板消息发送url
     */
    private static final String POST_JSON_SEND_TEMPLATE ="https://api.weixin.qq.com/cgi-bin/message/template/send?access_token=ACCESS_TOKEN";

    /**
     * 通过map发送模板消息
     *
     * @param accessToken 访问令牌
     * @param params      完整消息体
     * @return {@code ResponseBase }
     * @author booty
     * 
     */
    public static ResponseBase sendMsgByMap(String accessToken, Map<?,?> params){
        String url = POST_JSON_SEND_TEMPLATE.replace("ACCESS_TOKEN", accessToken);
        String result = HttpTool.doPostJson(url,JSON.toJSONString(params));
        ResponseBase response = JSON.parseObject(result, ResponseBase.class);
        response.check();
        return response;
    }

    @Data
    public static class KeyWord {
        /**
         * 消息
         */
        private String value;

        public KeyWord(String value) {
            this.value = value;
        }
    }

}
