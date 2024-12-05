# maven依赖

## 引入依赖

使用`dependencyManagement`管理依赖,避免版本冲突
```xml
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>mybatis-plus-enhancer</artifactId>
            <version>版本号</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>
```
按需引入相关依赖
```xml
<!--mybatis-plus-enhancer-->
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>mybatis-plus-enhancer</artifactId>
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

## 引入SNAPSHOT快照版本
配置快照仓库地址,正式版本无需配置, 会同步到中央仓库
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
若使用阿里云仓库, 需在maven的`settings.xml`文件中配置`!snapshots`以便拉取
```xml
<mirror>
  <id>aliyunmaven</id>
  <mirrorOf>*,!snapshots</mirrorOf>
  <name>aliyun</name>
  <url>https://maven.aliyun.com/repository/public</url>
</mirror>
```

# 代码生成器
## 导入包
```java
import io.github.bootystar.mybatisplus.generate.GeneratorHelper;
```
## 选择需要使用的生成器, 生成代码
```java
String url = "jdbc:postgresql://localhost:5432/test?useUnicode=true&characterEncoding=UTF-8";
String username = "postgres";
String password = "root";
GeneratorHelper
//        .extraCodeGenerator(url, username, password) // 额外代码生成器
//        .dynamicSqlGenerator(url, username, password) // 动态SQL生成器
        .dynamicFieldGenerator(url, username, password) // 动态字段生成器
        .initialize() // 初始化常用配置
        .pkg(pkg -> pkg.parent("io.github.bootystar" ))// 父包名
//        .mapperXmlResource("static/mapper") // mapper.xml文件在Resources下的路径
        .execute("sys_user" )// 要生成的表(不输入为全部)
;
```

## 可选配置项
```java
String url = "jdbc:postgresql://localhost:5432/test?useUnicode=true&characterEncoding=UTF-8";
String username = "postgres";
String password = "root";
GeneratorHelper
        // .extraCodeGenerator(url, username, password) // 额外代码生成器
        // .dynamicSqlGenerator(url, username, password) // 动态SQL生成器
        .dynamicFieldGenerator(url, username, password) // 动态字段生成器
        .enableGlobalFileOverwrite() // 全局文件覆盖生成(覆盖所有的文件)
        .mapperXmlResource("static/mapper") // mapper.xml文件在Resources下的路径
        .initialize() // 初始化常用配置
        .custom(custom -> {
            custom
                // 文件相关
                .enableFileOverride() // 文件覆盖生成(DTO、VO)
                .disableDocUUID() // 禁用文档UUID(swagger多个同名或空名对象会有冲突,使用uuid避免)
                .class4SelectDTO(Map.class) // 使用指定类作为查询入参DTO(推荐使用Map或SqlHelper)
                .package4DTO("dto") // DTO的包名
                .path4DTO("C:/Project/test21/") // DTO的路径(全路径或相对路径)
                .package4VO("vo") // VO的包名
                .path4VO("C:/Project/test21/") // VO的路径(全路径或相对路径)
                .editExcludeColumns("create_time", "update_time") // 新增/修改时忽略的字段
                // controller额外生成项
                .baseUrl("/api") // 请求url前缀
                .enableCrossOrigins() // 启用跨域
                .enableJakartaApi() // 启用Jakarta API, springboot3以上需要开启
                .enableAutoWired() // 使用@Autowired替换@Resource
                .returnMethod(R1::of) // 返回值对象封装的方法
                .pageMethod(P1::new) // 分页对象封装的方法
                .disableRestful() // 禁用restful
                .disableRequestBody() // 禁用请求体
                .disableValidated() // 禁用参数校验
                .disablePostQuery() // 复杂查询不使用post请求
                // mapper额外生成项
                .sortColumnClear() // 清空排序
                .sortColumn("create_time",true) // 添加排序(字段,是否倒序)
                .sortColumn("id",true) // 添加排序(字段,是否倒序)
                // 需要生成的方法
                .disableInsert() // 不生成新增
                .disableUpdate() // 不生成更新
                .disableDelete() // 不生成删除
                .disableSelect() // 不生成查询(若生成器为额外代码生成器并生成了导出, 则此项无效)
                .disableImport() // 不生成导入
                .disableExport() // 不生成导出
                // 特殊项, 因不同生成器而异
                .disableOverrideMethods() // 不生成重写的父类方法(动态字段生成器/动态sql生成器)
                .fieldSuffixBuilder(builder -> {
                // 该项默认无需配置, 配置后, 只会根据已配置的字段生成额外后缀, 未配置的类型不会生成后缀
                                builder
                                    .ne("Ne") // 不等于字段额外后缀
                                    .lt("Lt") // 小于字段额外后缀
                                    .le("Le") // 小于等于字段额外后缀
                                    .ge("Ge") // 大于等于字段额外后缀
                                    .gt("Gt") // 大于字段额外后缀
                                    .like("Like") // 模糊匹配字段额外后缀
                                    .notLike("NotLike") // 反模糊匹配字段额外后缀
                                    .in("In") // 包含字段额外后缀
                                    .notIn("NotIn") // 不包含字段额外后缀
                                    .isNull("IsNull") // 空字段额外后缀
                                    .isNotNull("IsNotNull") // 非空字段额外后缀
                    ;})// 额外自定义字段后缀(额外代码生成器/动态字段生成器专属)
                ;})
        .dataSource(dataSource -> {
        // 数据源配置(参考mybatis-plus官方文档)
        })
        .global(global -> {
        // 全局配置(参考mybatis-plus官方文档)
        })
        .pkg(pkg -> {
        // 包配置(参考mybatis-plus官方文档)
        })
        .strategy(strategy -> {
        // 策略配置(参考mybatis-plus官方文档)
        })
        .entity(entity -> {
        // 实体类配置(参考mybatis-plus官方文档)
        })
        .mapper(mapper -> {
        // mapper配置(参考mybatis-plus官方文档)
        })
        .service(service -> {
        // service配置(参考mybatis-plus官方文档)
        })
        .controller(controller -> {
        // controller配置(参考mybatis-plus官方文档)
        })
        .execute("sys_user") // 要生成的表(不输入为全部)
        ;
```
## 非链式调用配置

```java
String url = "jdbc:postgresql://localhost:5432/test?useUnicode=true&characterEncoding=UTF-8";
String username = "postgres";
String password = "root";
ExtraCodeGenerator generator = new ExtraCodeGenerator(url, username, password); // 额外代码生成器
// DynamicSqlGenerator generator = new DynamicSqlGenerator(url, username, password); // 动态SQL生成器
// DynamicFieldGenerator generator = new DynamicFieldGenerator(url, username, password); // 动态字段生成器

// 自定义配置
ExtraCodeConfig.Builder customConfigBuilder = generator.getCustomConfigBuilder();
customConfigBuilder
        .disableInsert() // 不生成新增
        .disableUpdate() // 不生成更新
        .disableDelete() // 不生成删除
        .disableSelect() // 不生成查询(若生成器为额外代码生成器并生成了导出, 则此项无效)
        .disableImport() // 不生成导入
        .disableExport() // 不生成导出
// ...略
;
// 字段后缀配置器
FieldSuffixBuilder fieldSuffixBuilder = customConfigBuilder.getFieldSuffixBuilder();
fieldSuffixBuilder
        .ne("Ne" ) // 不等于字段额外后缀
        .lt("Lt" ) // 小于字段额外后缀
        .le("Le" ) // 小于等于字段额外后缀
// ...略
;


// 数据源配置(参考mybatis-plus官方文档)
DataSourceConfig.Builder dataSourceConfigBuilder = generator.getDataSourceConfigBuilder();

// 全局配置(参考mybatis-plus官方文档)
GlobalConfig.Builder globalConfigBuilder = generator.getGlobalConfigBuilder();

// 包配置(参考mybatis-plus官方文档)
PackageConfig.Builder packageConfigBuilder = generator.getPackageConfigBuilder();

// 策略配置(参考mybatis-plus官方文档)
StrategyConfig.Builder strategyConfigBuilder = generator.getStrategyConfigBuilder();

// 实体类配置(参考mybatis-plus官方文档)
Entity.Builder entityBuilder = strategyConfigBuilder.entityBuilder();

// mapper配置(参考mybatis-plus官方文档)
Mapper.Builder mapperBuilder = strategyConfigBuilder.mapperBuilder();

// service配置(参考mybatis-plus官方文档)
Service.Builder serviceBuilder = strategyConfigBuilder.serviceBuilder();

// controller配置(参考mybatis-plus官方文档)
Controller.Builder controllerBuilder = strategyConfigBuilder.controllerBuilder();

// 要生成的表(不输入为全部)
generator.execute("sys_user");
```

# 运行时增强
