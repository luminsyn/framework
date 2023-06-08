package io.github.bootystar.wechat.officialAccount.entity;

import com.alibaba.fastjson2.JSON;
import io.github.bootystar.tool.HttpTool;
import io.github.bootystar.tool.ObjectTool;
import io.github.bootystar.wechat.common.exception.WechatResponseException;
import io.github.bootystar.wechat.entity.ResponseBase;
import lombok.Data;

/**
 *
 * 用户信息
 * @Author booty
 * @Date 2023/6/8 10:24
 */
@Data
public class UserInfo extends ResponseBase {

    /**
     * 用户的唯一标识
     */
    private String openid;
    /**
     * 用户昵称
     */
    private String nickname;
    /**
     * 用户的性别，值为1时是男性，值为2时是女性，值为0时是未知
     */
    private String sex;
    /**
     * 用户个人资料填写的省份
     */
    private String province;
    /**
     * 普通用户个人资料填写的城市
     */
    private String city;
    /**
     * 国家，如中国为CN
     */
    private String country;
    /**
     * 用户头像，最后一个数值代表正方形头像大小
     * （有0、46、64、96、132数值可选，0代表640*640正方形头像），
     * 用户没有头像时该项为空。
     * 若用户更换头像，原有头像URL将失效
     */
    private String headimgurl;
    /**
     * 用户特权信息，json 数组，如微信沃卡用户为（chinaunicom）
     */
    private String privilege;
    /**
     * 只有在用户将公众号绑定到微信开放平台帐号后，才会出现该字段。
     */
    private String unionid;

    private final static String GET_USER_INFO_URL="https://api.weixin.qq.com/sns/userinfo?access_token=ACCESS_TOKEN&openid=OPENID&lang=zh_CN";

    /**
     * 根据openId获取用户信息
     *
     * @param token  用户个人令牌
     * @param openId 开放id
     * @return {@code UserInfo }
     * @author booty
     * @date 2023/06/08 10:44
     */
    public static UserInfo getUserInfoByOpenId(String token,String openId){
        String url = GET_USER_INFO_URL.replace("ACCESS_TOKEN", token).replace("OPENID", openId);
         /*
        若出现乱码问题，不再使用HttpTool，启用此行代码
            try {
                url1 = new URL(url);
                urlConnection = (HttpURLConnection)url1.openConnection();
                try (InputStream inputStream = urlConnection.getInputStream();
                     InputStreamReader inputStreamReader = new InputStreamReader(inputStream, StandardCharsets.UTF_8);
                     BufferedReader in = new BufferedReader(inputStreamReader);
                ){
                    String result = in.readLine();
                    urlConnection.disconnect();
                    UserInfo userInfo = JSON.parseObject(result, UserInfo.class);
                    UserInfo userInfo = JSON.parseObject(result, UserInfo.class);
                    if (ObjectTool.isEmpty(userInfo.getOpenid())){
                        throw new WechatResponseException(userInfo);
                    }
                    return userInfo;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
         */
        String result = HttpTool.doGet(url);
        /*
        {
          "openid": "OPENID",
          "nickname": NICKNAME,
          "sex": 1,
          "province":"PROVINCE",
          "city":"CITY",
          "country":"COUNTRY",
          "headimgurl":"https://thirdwx.qlogo.cn/mmopen/g3MonUZtNHkdmzicIlibx6iaFqAc56vxLSUfpb6n5WKSYVY0ChQKkiaJSgQ1dZuTOgvLLrhJbERQQ4eMsv84eavHiaiceqxibJxCfHe/46",
          "privilege":[ "PRIVILEGE1" "PRIVILEGE2"     ],
          "unionid": "o6_bmasdasdsad6_2sgVt7hMZOPfL"
        }
         */
        UserInfo userInfo = JSON.parseObject(result, UserInfo.class);
        if (ObjectTool.isEmpty(userInfo.getOpenid())){
            throw new WechatResponseException(userInfo);
        }
        return userInfo;
    }

}
