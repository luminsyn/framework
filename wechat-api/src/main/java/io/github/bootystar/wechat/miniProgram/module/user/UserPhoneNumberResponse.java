package io.github.bootystar.wechat.miniProgram.module.user;

import io.github.bootystar.wechat.core.ResponseBase;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author bootystar
 * @since 2023/12/29
 */
@NoArgsConstructor
@Data
public class UserPhoneNumberResponse extends ResponseBase {


    /**
     * phone_info
     */
    private PhoneInfoDTO phone_info;

    /**
     * PhoneInfoDTO
     */
    @NoArgsConstructor
    @Data
    public static class PhoneInfoDTO {
        /**
         * phoneNumber
         */
        private String phoneNumber;
        /**
         * purePhoneNumber
         */
        private String purePhoneNumber;
        /**
         * countryCode
         */
        private Integer countryCode;
        /**
         * watermark
         */
        private WatermarkDTO watermark;

        /**
         * WatermarkDTO
         */
        @NoArgsConstructor
        @Data
        public static class WatermarkDTO {
            /**
             * timestamp
             */
            private Integer timestamp;
            /**
             * appid
             */
            private String appid;
        }
    }
}
