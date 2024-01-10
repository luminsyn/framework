package io.github.bootystar.wechat;

import io.github.bootystar.wechat.core.exception.ResponseException;
import io.github.bootystar.wechat.officialAccount.OfficialAccountApi;
import io.github.bootystar.wechat.core.token.AccessToken;
import io.github.bootystar.wechat.officialAccount.module.openApi.ResponseQueryQuota;
import io.github.bootystar.wechat.officialAccount.module.openApi.ResponseQueryRid;
import io.github.bootystar.wechat.officialAccount.module.web.UserAccessToken;
import io.github.bootystar.wechat.officialAccount.enums.CgiPathEnum;
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
        OfficialAccountApi oa = new OfficialAccountApi("11","111");
        oa.setAccessTokenFactory(() -> {
            AccessToken accessToken = new AccessToken();
            accessToken.setAccess_token("69__qr46W9JAA4d6rqxoNtuL8IvAs6kFjn7KOiifB7oLX_iFkzqzz2Stv5GdSC5s_oYsSSU5Cq85LnTrNMPr0W36FEsEOr4wPWRAWlE_ccBoiNgWhL9k3BHdAoPSRUSGCdAJAYMI");
            accessToken.setExpiresTime(LocalDateTime.now().plusSeconds(7200));
            return accessToken;
        });
        AccessToken accessToken = oa.getStableAccessToken();
        System.out.println(accessToken);

//        OfficialAccount wrongOa = new OfficialAccount("wxdcee357d389565c5", "0b190801e72f9db76512c978f99e79f");
//        try {
//            AccessToken token = wrongOa.getAccessToken();
//        }catch (WechatResponseException e){
//            ResponseBase responseBase = e.getResponseBase();
//            System.out.println(responseBase);
//            System.out.println(responseBase.getErrmsg());
//            System.out.println(responseBase.getErrcode());
//        }

        ResponseQueryRid rid = oa.queryRid("648567b6-3b612cab-67a23eba");
        System.out.println(rid);
        System.out.println(rid.getRequest());

//        QueryQuota queryQuota = oa.queryQuota("/cgi-bin/message/custom/send");
//        QueryQuota queryQuota = oa.queryQuota("/cgi-bin/clear_quota/v2 ");
        ResponseQueryQuota queryQuota = oa.queryQuota(CgiPathEnum.KF_ACCOUNT_UPDATE_HEAD_IMG);
        System.out.println(queryQuota);

        try {
            UserAccessToken wwawwanjkwndaknwjadwd = oa.createPersonalAccessToken("wwawwanjkwndaknwjadwd");
        }catch (ResponseException e){
            System.out.println(e.getResponseBase());
            System.out.println(e.getResponseBase().getErrcode());
            System.out.println(e.getResponseBase().getErrmsg());
        }



    }


}
