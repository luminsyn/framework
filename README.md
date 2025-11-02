# 框架集 (Framework Collection)


## 项目简介

用于公共配置的父 POM (Project Object Model)，旨在为多个项目提供统一的配置管理。

同时，此仓库也作为早期已弃用和拆分项目的提交记录存档，方便追溯历史变更。

## SNAPSHOT 仓库
```xml
<repositories>
  <repository>
    <name>Central Portal Snapshots</name>
    <id>central-portal-snapshots</id>
    <url>https://central.sonatype.com/repository/maven-snapshots/</url>
    <releases>
      <enabled>false</enabled>
    </releases>
    <snapshots>
      <enabled>true</enabled>
    </snapshots>
  </repository>
</repositories>
```

