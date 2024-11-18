# 封装的框架及工具
<font style="color:#ED740C;">详细文档见各模块的下的`README`文档</font>

maven仓库地址
```xml
<repositories>
    <repository>
        <id>snapshot</id>
        <name>snapshot</name>
        <url>https://s01.oss.sonatype.org/content/repositories/snapshots/</url>
        <snapshots>
            <enabled>true</enabled>
        </snapshots>
    </repository>
    <repository>
        <id>release</id>
        <name>release</name>
        <url>https://s01.oss.sonatype.org/content/repositories/releases/</url>
    </repository>
</repositories>
```
## helper
工具类及简化操作的实例化封装
## mybatis-plus-enhancer
基于mybatis-plus的代码生成器及运行时增强器, 增强了原有功能
## spring-boot-starter
自定义的spring-boot-starter,配置了一些默认参数, 简化项目搭建
## wechat-api
微信公众号/小程序的常用api封装




