package io.github.bootystar.wechat;

import io.github.bootystar.wechat.core.exception.ResponseException;
import io.github.bootystar.wechat.officialAccount.OfficialAccountApi;
import io.github.bootystar.wechat.core.token.AccessToken;
import io.github.bootystar.wechat.officialAccount.module.menu.ResponseMenuQuery;
import io.github.bootystar.wechat.officialAccount.module.openApi.ResponseQueryQuota;
import io.github.bootystar.wechat.officialAccount.module.openApi.ResponseQueryRid;
import io.github.bootystar.wechat.officialAccount.module.web.UserAccessToken;
import io.github.bootystar.wechat.officialAccount.enums.CgiPathEnum;
import io.github.bootystar.wechat.officialAccount.module.web.UserInfo;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

/**
 * 微信测试
 * @author booty
 * 
 */
public class WechatTest {

    @Test
    void test1(){
        OfficialAccountApi oa = new OfficialAccountApi("123","123");
        
        UserAccessToken personalAccessToken = oa.createPersonalAccessToken("001C2VFa1PTWaH0N8TGa1bJg9F3C2VF7");
        UserInfo userInfoByOpenId = oa.getUserInfoByOpenId(personalAccessToken,personalAccessToken.getOpenid());
        System.out.println();

    }





}
