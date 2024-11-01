package io.github.bootystar.wechat.officialAccount.module.openApi;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import io.github.bootystar.wechat.tool.HttpTool;
import io.github.bootystar.wechat.core.ResponseBase;

/**
 * openApi管理
 * @author bootystar
 * 
 */
public class OpenApi {

    private OpenApi() {
    }

    /**
     * 本接口用于清空公众号/小程序/第三方平台等接口的每日调用接口次数
     */
    private static final String POST_JSON_CLEAR_QUOTA_URL ="https://api.weixin.qq.com/cgi-bin/clear_quota?access_token=ACCESS_TOKEN";

    /**
     * 清空公众号/小程序/第三方平台等接口的每日调用接口次数
     *
     * 注意事项
     * 1、如果要清空公众号的接口的quota，则需要用公众号的access_token；如果要清空小程序的接口的quota，则需要用小程序的access_token；如果要清空第三方平台的接口的quota，则需要用第三方平台的component_access_token
     *
     * 2、如果是第三方服务商代公众号或者小程序清除quota，则需要用authorizer_access_token
     *
     * 3、每个帐号每月共10次清零操作机会，清零生效一次即用掉一次机会；第三方帮助公众号/小程序调用时，实际上是在消耗公众号/小程序自身的quota
     *
     * 4、由于指标计算方法或统计时间差异，实时调用量数据可能会出现误差，一般在1%以内
     * @return boolean
     * @author bootystar
     * 
     */
    public static ResponseBase clearQuotaByAccessToken(String appId, String accessToken){
        String url = POST_JSON_CLEAR_QUOTA_URL.replace("ACCESS_TOKEN", accessToken);
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("appid",appId);
        String body = jsonObject.toJSONString();
        String result = HttpTool.doPostJson(url, body);
        ResponseBase response = JSON.parseObject(result, ResponseBase.class);
        response.check();
        return response;
    }

    /**
     * 使用AppSecret重置 API 调用次数
     */
    private static final String POST_CLEAR_QUOTA_V2_URL="https://api.weixin.qq.com/cgi-bin/clear_quota/v2?appid=APPID&appsecret=APPSECRET";

    /**
     * 本接口用于清空公众号/小程序等接口的每日调用接口次数
     *
     * 注意事项
     * 1、该接口通过appsecret调用，解决了accesss_token耗尽无法调用重置 API 调用次数的情况
     *
     * 2、每个帐号每月使用重置 API 调用次数 与本接口共10次清零操作机会，清零生效一次即用掉一次机会；
     *
     * 3、由于指标计算方法或统计时间差异，实时调用量数据可能会出现误差，一般在1%以内
     *
     * 4、该接口仅支持POST调用
     *
     * @param appId     应用程序id
     * @param appSecret 应用程序秘密
     * @return {@code ResponseBase }
     * @author bootystar
     * 
     */
    public static ResponseBase clearQuotaByAppSecret(String appId, String appSecret){
        String url = POST_CLEAR_QUOTA_V2_URL.replace("APPID", appId).replace("APPSECRET", appSecret);
        String result = HttpTool.doPostForm(POST_CLEAR_QUOTA_V2_URL,null);
        ResponseBase response = JSON.parseObject(result, ResponseBase.class);
        response.check();
        return response;
    }



    /**
     * 本接口用于查询公众号/小程序/第三方平台等接口的每日调用接口的额度以及调用次数
     */
    private static final String POST_JSON_QUERY_QUOTA_URL ="https://api.weixin.qq.com/cgi-bin/openapi/quota/get?access_token=ACCESS_TOKEN";

    /**
     * 查询openAPI调用配额
     *
     * 注意事项
     * 1、如果查询的api属于公众号的接口，则需要用公众号的access_token；如果查询的api属于小程序的接口，则需要用小程序的access_token；如果查询的接口属于第三方平台的接口，则需要用第三方平台的component_access_token；否则会出现76022报错。
     *
     * 2、如果是第三方服务商代公众号或者小程序查询公众号或者小程序的api，则需要用authorizer_access_token
     *
     * 3、每个接口都有调用次数限制，请开发者合理调用接口
     *
     * 4、”/xxx/sns/xxx“这类接口不支持使用该接口，会出现76022报错。
     *
     * @param accessToken 访问令牌
     * @param cgiPath     cgi路径
     * @return {@code QueryQuota }
     * @author bootystar
     * 
     */
    public static ResponseQueryQuota queryQuota(String accessToken, String cgiPath){
        String url = POST_JSON_QUERY_QUOTA_URL.replace("ACCESS_TOKEN", accessToken);
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("cgi_path",cgiPath);
        String body = jsonObject.toJSONString();
        String result = HttpTool.doPostJson(url, body);
        /*
        {
          "errcode": 0,
          "errmsg": "ok",
          “quota”:{
            "daily_limit": 0,
            "used": 0,
            "remain": 0}
        }
         */
        ResponseQueryQuota response = JSON.parseObject(result, ResponseQueryQuota.class);
        response.check();
        return response;
    }




    private static final String POST_JSON_QUERY_RID ="https://api.weixin.qq.com/cgi-bin/openapi/rid/get?access_token=ACCESS_TOKEN";
    /**
     * 查询错误信息
     *
     * @param accessToken 访问令牌
     * @param rid         错误rid
     * @return {@code Rid }
     * @author bootystar
     * 
     */
    public static ResponseQueryRid queryRid(String accessToken, String rid){
        String url = POST_JSON_QUERY_RID.replace("ACCESS_TOKEN", accessToken);
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("rid",rid);
        String body = jsonObject.toJSONString();
        String result = HttpTool.doPostJson(url, body);
        /*
         {
              "errcode":0,
              "errmsg":"ok",
              "request":{
                  "invoke_time":1635156704,
                  "cost_in_ms":30,
                  "request_url":"access_token=50_Im7xxxx",
                  "request_body":"",
                  "response_body":"{\"errcode\":45009,\"errmsg\":\"reach max api daily quota limit rid: 617682e0-09059ac5-34a8e2ea\"}",
                  "client_ip": "113.xx.70.51"
              }
          }
         */
        ResponseQueryRid response = JSON.parseObject(result, ResponseQueryRid.class);
        response.check();
        return response;
    }



}
