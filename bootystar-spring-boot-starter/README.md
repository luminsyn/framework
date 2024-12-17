# maven依赖

## 引入依赖

```xml
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>bootystar-spring-boot-starter</artifactId>
    <version>1.0.0</version>
</dependency>
```
# 自动配置项

## spring
* 添加`LocalDateTime`,`LocalDate`,`LocalTime`的`Converter`

## jackson
* 自动配置时间格式`yyyy-MM-dd HH:mm:ss`
* 自动配置时区为`GMT+8`
* 禁止将 `java.util.Date`、`Calendar` 序列化为数字(时间戳)
* 设置 `java.util.Date`, `Calendar` 序列化、反序列化的时区
* 自动配置`LocalDateTime`,`LocalDate`,`LocalTime`的序列化和反序列化
* `Long`类型序列化为`String`, 解决前端精度丢失问题
* 序列化对象为`null`时不抛出异常
* 反序列化`JSON`中包含pojo不存在属性时不抛出异常

## mybatis-plus
* 自动配置mybatis-plus分页插件
* 自动配置mybatis-plus乐观锁插件
* 自动配置mybatis-plus防全表更新插件

# 功能项

## JSON字段注解加解密
