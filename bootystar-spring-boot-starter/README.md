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

## JSON注解加解密

该功能基于`jackson`实现, 若自定义配置了其他序列化框架, 该功能无效

### 指定对应类型的处理逻辑
* 编写类, 实现`java.util.function.Function<T, R>`接口
* 根据目标`属性类型`及实际需求, 编写序列化逻辑或反序列化逻辑
* 若为序列化, 泛型`T`为属性的类型, 泛型`R`为`String` 
* 若为反序列化, 泛型`T`为`String`, 泛型`R`为属性的类型

示例 : `LocalDate`类型的序列化类
```java
public class DateOut implements Function<LocalDate, String> {
    @Override
    public String apply(LocalDate s) {
        // 将指定类型的对象转化为字符串
        return s.toString() + "转化后的日期出参";
    }
}
```
示例 : `LocalDate`类型的反序列化类
```java
public class DateIn implements Function<String, LocalDate> {
    private final static 
    @Override
    public LocalDate apply(String s) {
        // 将字符串转化为指定类型的对象,
        return LocalDate.parse(s , DateTimeFormatter.ofPattern("yyyy年MM月dd"));
    }
}
```
### 在属性上添加注解
* `@JsonEnc`注解
* `serialize`: 序列化时的处理类
* `deserialize`: 反序列化时的处理类
* 可以单指定`serialize`或`deserialize`, 也可以同时指定
* 若未指定, 注解不会生效
* `@JsonEnc`优先级高于`@JsonFormat`注解, 若同时存在, `@JsonEnc`生效, 当`@JsonEnc`无效时, `@JsonFormat`注解生效
```java
public class Entity {
    
    @JsonEnc(serialize = DateOut.class) // 序列化时使用DateOut类处理
    private LocalDate date;
    
    @JsonEnc(deserialize = DateIn.class) // 反序列化时使用DateIn类处理
    private LocalDate date;

    @JsonEnc(serialize = DateOut.class, deserialize = DateIn.class) // 序列化和反序列化时使用DateOut和DateIn类处理
    private LocalDate date3;
}
```
