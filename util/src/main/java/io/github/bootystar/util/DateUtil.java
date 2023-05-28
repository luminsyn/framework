package io.github.bootystar.util;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 * 日期操作工具
 * @author booty
 * @date 2023/5/28 11:15
 */
public class DateUtil {
    /**
     * 格式化
     * DateTimeFormatter
     *
     * YYYY 代表 Week Year
     * yyyy 代表year
     *
     * MM 代表 月（Month）
     * mm代表 秒（Min）
     *
     * DD  格式是指当前日期在当年中的天数
     * dd  当月的天
     *
     * HH代表24小时制
     * hh代表12小时制
     */
    public static final String  DATE_TIME_EXPRESSION="yyyy-MM-dd HH:mm:ss";
    public static final String  DATE_EXPRESSION="yyyy-MM-dd";
    public static final String  TIME_EXPRESSION="HH:mm:ss";

    private static final ZoneId ZONE_ID = ZoneId.systemDefault();
    private static final SimpleDateFormat SDF_DATE_TIME=new SimpleDateFormat(DATE_TIME_EXPRESSION);
    private static final SimpleDateFormat SDF_DATE=new SimpleDateFormat(DATE_TIME_EXPRESSION);
    private static final SimpleDateFormat SDF_TIME=new SimpleDateFormat(DATE_TIME_EXPRESSION);
    private static final DateTimeFormatter DTF_LOCAL_DATE_TIME = DateTimeFormatter.ofPattern(DATE_TIME_EXPRESSION);
    private static final DateTimeFormatter DTF_LOCAL_DATE = DateTimeFormatter.ofPattern(DATE_EXPRESSION);
    private static final DateTimeFormatter DTF_LOCAL_TIME = DateTimeFormatter.ofPattern(TIME_EXPRESSION);


    /**
     * Date转LocalDate
     * @param date
     * @return
     */
    public static LocalDate date2LocalDate(Date date){
        return date.toInstant().atZone(ZONE_ID).toLocalDate();
    }

    /**
     * Date转LocalDate
     * @param date
     * @param zoneId  时区id
     * @return
     */
    public static LocalDate date2LocalDate(Date date,ZoneId zoneId){
        return date.toInstant().atZone(zoneId).toLocalDate();
    }

    /**
     * Date转LocalTime
     * @param date
     * @return
     */
    public static LocalTime date2DateTime(Date date){
        return date.toInstant().atZone(ZONE_ID).toLocalTime();
    }

    /**
     * Date转LocalTime
     * @param date
     * @param zoneId  时区id
     * @return
     */
    public static LocalTime date2DateTime(Date date,ZoneId zoneId){
        return date.toInstant().atZone(zoneId).toLocalTime();
    }

    /**
     * Date转LocalDateTime
     * @param date
     * @return
     */
    public static LocalDateTime date2LocalDateTime(Date date){
        return date.toInstant().atZone(ZONE_ID).toLocalDateTime();
    }
    /**
     * Date转LocalDateTime
     * @param date
     * @param zoneId  时区id
     * @return
     */
    public static LocalDateTime date2LocalDateTime(Date date,ZoneId zoneId){
        return date.toInstant().atZone(ZONE_ID).toLocalDateTime();
    }

    /**
     * LocalDateTime转Date
     * @param local
     * @return
     */
    public static Date localDateTime2Date(LocalDateTime local){
        ZonedDateTime zdt = local.atZone(ZONE_ID);
        return Date.from(zdt.toInstant());
    }

    /**
     * LocalDateTime转Date
     * @param local
     * @return  指定时间+今日日期拼接的Date
     */
    public static Date localTime2Date(LocalTime local){
        LocalDate localDate = LocalDate.now();
        LocalDateTime localDateTime = LocalDateTime.of(localDate, local);
        Instant instant = localDateTime.atZone(ZONE_ID).toInstant();
        java.util.Date date = Date.from(instant);
        return date;
    }

    /**
     * LocalDate转Date
     * @param local
     * @return  今日日期0时0分0秒对应的Date
     */
    public static Date localDate2Date(LocalDate local){
        ZonedDateTime zdt = local.atStartOfDay(ZONE_ID);
        return Date.from(zdt.toInstant());
    }

    /**
     * 字符串转Date
     * 格式：yyyy-MM-dd HH:mm:ss
     * @param source
     * @return
     */
    public static Date string2DateTime(String source) {
        try {
            Date date = SDF_DATE_TIME.parse(source);
            return date;
        } catch (ParseException e) {
            throw new RuntimeException("字符格式不匹配");
        }
    }

    /**
     * 字符串转Date
     * 格式：yyyy-MM-dd
     * @param source
     * @return
     */
    public  static Date string2Date(String source) {
        try {
            LocalTime now = LocalTime.now();
            String format = DTF_LOCAL_TIME.format(now);
            Date date = SDF_DATE.parse(source+ " 00:00:00");
            return date;
        } catch (ParseException e) {
            throw new RuntimeException("字符格式不匹配");
        }
    }

    /**
     * 字符串转Time
     * 格式：HH:mm:ss
     * @param source
     * @return
     */
    public static Date string2Time(String source) {
        try {
            LocalDate now = LocalDate.now();
            String format = DTF_LOCAL_DATE.format(now);
            Date date = SDF_TIME.parse(format+" "+source);
            return date;
        } catch (ParseException e) {
            throw new RuntimeException("字符格式不匹配");
        }
    }


    public static Date string2Date(String source,String pattern) {
        try {
            SimpleDateFormat sdf = new SimpleDateFormat(pattern);
            Date date = sdf.parse(pattern);
            return date;
        } catch (ParseException e) {
            throw new RuntimeException("字符格式不匹配");
        }
    }


}
