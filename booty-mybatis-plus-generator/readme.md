# 环境依赖
## 1.导入生成器依赖
```xml
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>booty-mybatis-plus-generator</artifactId>
            <version>1.0.2</version>
        </dependency>
```
注: 生成器默认不传递依赖, 需要使用需自行引入mybatis-plus, mysql 及easyexcel等需要使用的组件, 请自行导入
## 2.需自行导入的相关依赖
```xml
        <dependency>
            <groupId>com.baomidou</groupId>
            <artifactId>mybatis-plus-boot-starter</artifactId>
            <version>3.5.3.1</version>
        </dependency>
        <dependency>
            <groupId>mysql</groupId>
            <artifactId>mysql-connector-java</artifactId>
            <version>8.0.32</version>
        </dependency>
        <!--需要处理excel时引入-->
        <dependency>
            <groupId>com.alibaba</groupId>
            <artifactId>easyexcel</artifactId>
            <version>3.3.3</version>
        </dependency>
```
## 3.可选依赖

- `booty-helper` 
   - 内含Excel转化器处理工具
- `booty-spring-boot-starter`
   - 自动配置jackson中关于LocalDateTime\Long\Double的序列化格式
   - 若引入了`booty-helper`和`easyexcel`依赖, 自动配置esayexcel常用类型的转化并添加不支持的类型(如java8time包)的转化器
```xml
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>booty-spring-boot-starter</artifactId>
            <version>1.0.2</version>
        </dependency>
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>booty-helper</artifactId>
            <version>1.0.2</version>
        </dependency>
```
# 使用方式
代码生成器分为两种

- 继承生成器(推荐)
   - 优点: 方法由父类实现, 可自由重写, 扩展性高, 可读性高
   - 缺点: 运行时需要`booty-mybatis-plus-generator`依赖
- CRUD生成器
   - 优点: 无侵入, 使用该生成器生成的代码, 仅依赖mybatis-plus, 运行时无需自身依赖
   - 缺点: 文件内代码代码较多, 冗余较高, 文件关联较强
## 1.继承生成器
```java
String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
String username ="root";
String password ="root";
String projectPath = System.getProperty("user.dir");
ParentGenerator generator = new ParentGenerator(url, username, password);
// 生成指定表
generator.execute("user","role");
// 生成所有表
// generator.execute();
```
## 2.CRUD生成器
```java
String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
String username ="root";
String password ="root";
String projectPath = System.getProperty("user.dir");
CrudGenerator generator = new CrudGenerator(url, username, password);
// 生成指定表
generator.execute("user","role");
// 生成所有表
// generator.execute();
```
## 3.可选配置项
默认兼容mybatis-plus的所有原配置项
自定义配置项单独使用`CustomConfigBuilder`配置
```java
String url ="jdbc:mysql://localhost:3306/test?useUnicode=true&characterEncoding=UTF-8";
String username ="root";
String password ="root";
ParentGenerator generator = new ParentGenerator(url, username, password);

//mybatis-plus-包配置
generator
    .packageConfigBuilder()
    .parent("com.xxx.xxx") //包名
    .entity("entity") // 实体类所在包
    .pathInfo(Collections.singletonMap(OutputFile.xml, projectPath + "/aa-test/src/main/resources/mapper")) // 指定xml输出目录为resources/mapper
;
//mybatis-plus-实体类配置
generator.strategyConfigBuilder().entityBuilder()
    .enableLombok() // lombok样式
;

// 自定义配置项目
generator.customConfigBuilder()
    .jakartaApi(false) // jakarta包替换javax包(jdk17及以上)
    .DTOPackage("entity.dto") // dto所在包
    .VOPackage("entity.vo") // vo所在包    
    .insertExcludeFields(Arrays.asList("create_time")) // 新增排除字段(默认添加createTime,updateTime,重新设置后失效)
    .updateExcludeFields(Arrays.asList("create_time")) // 新增排除字段(默认添加createTime,updateTime,重新设置后失效)
    .requestBody(false) // controller是否使用@RequestBody接收参数
    .enableOrigins(true) // controller添加@Origin注解(允许跨域)
    .allPost(true) // controller所有请求都采用post
    .baseUrl("/test") // controller请求前缀
    .restStyle(true) // restful样式请求(id路径传参)
    .returnMethod(Result::success) // 指定controller返回的实体类以及静态方法或构造器
    .removeReturnMethod() // 移除配置的返回实体类

    .resultMapForVO(true) // 为vo生成resultMap
    .exportOnVO(true) // 直接使用vo作为导出类
    .importOnVO(true) // 直接使用vo作为导入类
    .fieldAnnotationOnVO(true)// vo上添加属性注释

    .orderColumn("age",true) // 添加排序字段(默认添加create_time和id倒排, 可通过orderColumnMap传入空map清除)
    .orderColumn("name", false) // 添加排序字段
    .orderColumn("id_card", true) // 添加排序字段

    .generateInsert(true) // 是否生成新增方法(默认true)
    .generateUpdate(true) // 是否生成更新方法(默认true)
    .generateDelete(true) // 是否生成删除方法(默认true)
    .generateSelect(true) // 是否生成查询方法(默认true)
    .generateExport(true) // 是否生成导出方法(默认true)
    .generateImport(true) // 是否生成导入方法(默认true)


    .fileOverride(true) // 覆盖生成vo,dto
;

// 要生成的表名
generator.execute("user");
```
