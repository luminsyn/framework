# maven依赖
## 配置快照仓库地址
按需配置,正式版本会同步到中央仓库
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
## 引入maven依赖

<font style="color:#ED740C;">注:引入mybatis-plus相关依赖时无需指定版本</font>

```mvn
<!--mybatis-plus-enhancer-->
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>mybatis-plus-enhancer</artifactId>
    <version>0.0.1-SNAPSHOT</version>
</dependency>

<!-- spring boot3 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-spring-boot3-starter</artifactId>
</dependency>

<!-- spring boot2 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-boot-starter</artifactId>
</dependency>


<!-- jdk 11+ 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-jsqlparser</artifactId>
</dependency>

<!-- jdk 8+ 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-jsqlparser-4.9</artifactId>
</dependency>
```


# 代码生成器

代码生成器所在包为
```java
import io.github.bootystar.mybatisplus.generator.*;
```

## <font style="background-color:rgba(255, 255, 255, 0);">SimpleGenerator</font>
+ <font style="color:#ED740C;">兼容性好</font>
+ 生成时需要该依赖, 运行时仅依赖于mybatisplus
+ <font style="background-color:rgba(255, 255, 255, 0);">直接将额外代码嵌入原有类中</font>
+ <font style="color:#117CEE;background-color:rgba(255, 255, 255, 0);">嵌入代码量大, 重复性高</font>
+ <font style="color:#117CEE;background-color:rgba(255, 255, 255, 0);">类添加/修改字段后, mapper需要添加/修改字段对应的查询参数</font>

## <font style="background-color:rgba(255, 255, 255, 0);">CustomGenerator</font>
+ <font style="color:#ED740C;">最简洁</font>
+ 生成/运行时都需要该依赖及mybatisplus
+ <font style="background-color:rgba(255, 255, 255, 0);">逻辑继承自父类, 无额外代码</font>
+ <font style="background-color:rgba(255, 255, 255, 0);">查询方法会生成到xml文件中, 可用查询参数会封装到查询DTO中</font>
+ <font style="color:#117CEE;background-color:rgba(255, 255, 255, 0);">类添加/修改字段后, mapper和查询DTO需要添加/修改字段对应的查询参数</font>

## <font style="background-color:rgba(255, 255, 255, 0);">SplicingGenerator</font>
+ <font style="color:#ED740C;">最灵活</font>
+ 生成/运行时都需要该依赖及mybatisplus
+ <font style="background-color:rgba(255, 255, 255, 0);">逻辑继承自父类, 无额外代码</font>
+ <font style="background-color:rgba(255, 255, 255, 0);">可任意自定义参数/查询类型/值, 可自定义排序</font>
+ <font style="background-color:rgba(255, 255, 255, 0);">类字段修改后无需添加mapper和DTO, 会自动根据实体类适配</font>
+ <font style="color:#117CEE;background-color:rgba(255, 255, 255, 0);">前端传参较复杂, 需要指定: 字段/类型/对应值</font>

# 使用方式
## 最简使用
```java
String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
String username ="root";
String password ="root";

//SimpleGenerator generator = new SimpleGenerator(url, username, password); // 简单生成器, 最兼容, 直接将额外代码嵌入原有类中,不添加额外依赖,提供默认查询参数
//CustomGenerator generator = new CustomGenerator(url, username, password); // 自定义生成器, 最简洁, 继承父类实现,提供默认查询参数
SplicingGenerator generator = new SplicingGenerator(url, username, password); // SQL注入生成器, 最灵活, 添加防注入措施,运行时可自定义任何字段的查询参数,但前端传参较复杂

// 生成指定表
generator.execute("user");
```

## 推荐配置
使用`initialize()`方法一键配置常用配置项

推荐配置`包名`,`xml文件路径`,`返回值封装方法`,`分页封装方法`

并根据项目实际修改`请求前缀`,`跨域注解`,`Jakarta包`,`restful风格`,`复杂查询使用post`

```java
String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
String username ="root";
String password ="root";

//        SimpleGenerator generator = new SimpleGenerator(url, username, password); // 简单生成器, 最兼容, 直接将额外代码嵌入原有类中,不添加额外依赖,提供默认查询参数
//        CustomGenerator generator = new CustomGenerator(url, username, password); // 自定义生成器, 最简洁, 继承父类实现,提供默认查询参数
SplicingGenerator generator = new SplicingGenerator(url, username, password); // SQL注入生成器, 最灵活, 添加防注入措施,运行时可自定义任何字段的查询参数,但前端传参较复杂

generator
        .initialize() // 一键配置常用配置项
//      .mapperXmlResource("static/mapper") // xml文件在resource目录下的路径(默认在mapper目录下)
; 

generator.customConfigBuilder()
//        .baseUrl("/api") // controller请求前缀
//        .returnMethod(R::new) // 返回值的封装方法
//        .enableOrigins() // 开启跨域
//        .pageMethod(P::new) // 分页的封装方法
//        .enableJakartaApi() // 开启Jakarta API(替换javax包, springboot3及之后版本默认使用Jakarta包)
//        .disableRestful() // 禁止restful风格
//        .disablePostOnComplicatedSelect() // 禁止复杂查询使用post请求
;


// 以下为mybatis-plus generator官方提供配置,默认无需配置,若需配置请参考官方文档
generator.globalConfigBuilder() // 全局配置
//                .author("bootystar") // 作者名称,会自动根据电脑用户名称获取
//                .outputDir(System.getProperty("user.dir") + "/src/main/java") // 默认生成到项目目录下
;

generator.packageConfigBuilder() //包设置
                .parent("io.github.bootystar") // 父包名, 建议修改
;


// 生成指定表
generator.execute("user");

// 生成所有表
// generator.execute();
```

## 按需配置自定义参数
```java
String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
String username ="root";
String password ="root";

//SimpleGenerator generator = new SimpleGenerator(url, username, password); // 简单生成器, 最兼容, 直接将额外代码嵌入原有类中,不添加额外依赖,提供默认查询参数
//CustomGenerator generator = new CustomGenerator(url, username, password); // 自定义生成器, 最简洁, 继承父类实现,提供默认查询参数
SplicingGenerator generator = new SplicingGenerator(url, username, password); // SQL注入生成器, 最灵活, 添加防注入措施,运行时可自定义任何字段的查询参数,但前端传参较复杂

generator
        .initialize() // 一键配置常用配置项
//      .mapperXmlResource("static/mapper") // xml文件在resource目录下的路径(也可通过包配置自行配置)
; 


generator.customConfigBuilder()
        // 通用设置
        .DTOPackage("dto") // DTO包名, 默认dto
        .VOPackage("vo") // VO包名, 默认vo
        .insertExcludeFields(Arrays.asList("createTime", "updateTime")) // 新增DTO忽略的字段
        .updateExcludeFields(Arrays.asList("createTime", "updateTime")) // 更新DTO忽略的字段
        .enableFileOverride() // 开启DTO\VO的文件覆盖
        .enableFieldAnnotationOnVO() // 在VO上添加@Tablefield属性注释
        .disableExportOnVO() // 禁用导出使用VO, 额外生成导出DTO
        .disableImportOnVO() // 禁用导入使用VO, 额外生成导入DTO
        .disableInsert() // 不生成新增方法及DTO
        .disableUpdate() // 不生成更新方法及DTO
        .disableSelect() // 不生成查询方法及DTO
        .disableExport() // 不生成导出方法及DTO
        .disableImport() // 不生成导入方法及DTO
        .disableSelect() // 不生成查询方法()

        // controller相关设置
        .baseUrl("/api") // controller请求前缀
        .enableOrigins() // 开启跨域
        .returnMethod(R::new) // 返回值的封装方法
        .pageMethod(P::new) // 分页的封装方法
        .enableJakartaApi() // 开启Jakarta API(替换javax包, springboot3及之后版本默认使用Jakarta包)
        .disableRestful() // 开启restful风格
        .disableRequestBody() // 禁用requestBody接收参数
        .disableValidated() // 禁用参数校验
        .disablePostOnComplicatedSelect() // 禁止复杂查询使用post请求

        // mapper设置
        .enableResultMapForVO() // 开启VO的结果集封装
        .orderColumnMap(new HashMap<>()) // 指定排序字段map集,如需清空,传入new HashMap<>()或null即可清空
        .orderColumn("sort", true) // 添加排序字段(不会清空已添加的)

        // CustomGenerator和SplicingGenerator特有,SimpleGenerator无该配置项
        .disableServiceImplOverrideMethod() // 显示ServiceImpl重写的父类方法
        .disableMapperOverrideMethod() // 显示Mapper重写的父类方法
;

// 生成指定表
generator.execute("user");

// 生成所有表
// generator.execute();
```

## 按需配置mybatis-plus参数
```java
String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
String username ="root";
String password ="root";

//        SimpleGenerator generator = new SimpleGenerator(url, username, password); // 简单生成器, 最兼容, 直接将额外代码嵌入原有类中,不添加额外依赖,提供默认查询参数
//        CustomGenerator generator = new CustomGenerator(url, username, password); // 自定义生成器, 最简洁, 继承父类实现,提供默认查询参数
SplicingGenerator generator = new SplicingGenerator(url, username, password); // SQL注入生成器, 最灵活, 添加防注入措施,运行时可自定义任何字段的查询参数,但前端传参较复杂

// generator.mapperXmlResource("static/mapper"); // xml文件在resource目录下的路径

generator.customConfigBuilder()
        // 通用设置
        .DTOPackage("dto") // DTO包名, 默认dto
        .VOPackage("vo") // VO包名, 默认vo
        .insertExcludeFields(Arrays.asList("createTime", "updateTime")) // 新增DTO忽略的字段,默认添加了createTime和updateTime
        .updateExcludeFields(Arrays.asList("createTime", "updateTime")) // 更新DTO忽略的字段,默认添加了createTime和updateTime
        .enableFileOverride() // 开启DTO\VO的文件覆盖
        .enableFieldAnnotationOnVO() // 在VO上添加@Tablefield属性注释
        .disableExportOnVO() // 禁用导出使用VO, 额外生成导出DTO
        .disableImportOnVO() // 禁用导入使用VO, 额外生成导入DTO
        .disableInsert() // 不生成新增方法及DTO
        .disableUpdate() // 不生成更新方法及DTO
        .disableSelect() // 不生成查询方法及DTO
        .disableExport() // 不生成导出方法及DTO
        .disableImport() // 不生成导入方法及DTO
        .disableSelect() // 不生成查询方法()

        // controller相关设置
        .baseUrl("/api") // controller请求前缀
        .enableOrigins() // 开启跨域
        .returnMethod(R::new) // 返回值的封装方法
        .pageMethod(P::new) // 分页的封装方法
        .enableJakartaApi() // 开启Jakarta API(替换javax包, springboot3及之后版本默认使用Jakarta包)
        .disableRestful() // 开启restful风格
        .disableRequestBody() // 禁用requestBody接收参数
        .disableValidated() // 禁用参数校验
        .disablePostOnComplicatedSelect() // 复杂查询使用post请求替换get

        // mapper设置
        .enableResultMapForVO() // 开启VO的结果集封装
        .orderColumnMap(new HashMap<>()) // 指定排序字段map集,默认已添加create_time和id, 如需清空,传入new HashMap<>()或null即可清空
        .orderColumn("sort", true) // 添加排序字段(不会清空已添加的)

        // CustomGenerator和SplicingGenerator特有,SimpleGenerator无该配置项
        .disableServiceImplOverrideMethod() // 显示ServiceImpl重写的父类方法
        .disableMapperOverrideMethod() // 显示Mapper重写的父类方法
;


// 以下为mybatis-plus generator官方提供配置,默认无需配置,若需配置请参考官方文档
generator.globalConfigBuilder() // 全局配置

;

generator.packageConfigBuilder() //包设置

;

generator.strategyConfigBuilder().entityBuilder() //实体类设置

;
generator.strategyConfigBuilder().controllerBuilder() // controller设置

;
generator.strategyConfigBuilder().serviceBuilder() // service设置

;
generator.strategyConfigBuilder().mapperBuilder() //mapper相关设置

;


// 生成指定表
generator.execute("user");

// 生成所有表
// generator.execute();
```

# 核心逻辑
## 继承的父类<font style="background-color:rgba(255, 255, 255, 0);">GenericService的</font>逻辑
![画板](https://cdn.nlark.com/yuque/0/2024/jpeg/12797324/1731468926109-e985a5a5-ec08-4b10-b84b-db7983736e0b.jpeg)

### Splicer反注入自定义参数
使用`io.github.bootystar.mybatisplus.logic.splicing.dto.Splicer`作为入参
+ 调用`ImmutableSplicer()`会自动根据传入实体类反注入
+ 调用`requiredConditions()`会自动添加优先级更高的条件, 添加后的条件必定生效
+ 调用`addConditions()`会添加条件集

<font style="background-color:rgba(255, 255, 255, 0);">反注入过程</font>

1. <font style="background-color:rgba(255, 255, 255, 0);">通过反射获取指定实体类的所有可用字段</font>
2. <font style="background-color:rgba(255, 255, 255, 0);">检索mybatis-plus对应注解并处理数据库字段</font>
3. <font style="background-color:rgba(255, 255, 255, 0);">若实体类实现了`io.github.bootystar.mybatisplus.logic.splicing.SplicingEntity`接口,根据实现方法添加额外字段</font>

### 自定义连表查询
在mapper中添加需要连表的表名, 并添加别名

![](https://cdn.nlark.com/yuque/0/2024/png/12797324/1731469649819-3bb8af84-87df-45a4-895b-d732e8948300.png)

在实体类中指定需要连表的字段

+ 添加属性, 在属性上使用`@TableField`注解, 并指定`exist=false`, 指定`value`为指定表字段
+ 实现SplicingEntity接口, 并在返回的map中添加属性名和对应表字段名

![](https://cdn.nlark.com/yuque/0/2024/png/12797324/1731469795547-bfdf8b86-aa7f-4e2a-aeea-ff8d947effc0.png)

