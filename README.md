# 框架集 (Framework Collection)


## 项目简介

用于公共配置的父 POM (Project Object Model)，旨在为多个项目提供统一的配置管理。

同时，此仓库也作为早期已弃用和拆分项目的提交记录存档，方便追溯历史变更。

## 主要特性

*   **统一配置管理**：通过父 POM 集中管理公共依赖和插件版本。
*   **历史记录追溯**：保留了早期项目的完整提交历史，便于审计和参考。

## 快速开始

```xml
<dependency>
    <groupId>io.github.luminsyn</groupId>
    <artifactId>framework</artifactId>
    <version>latest</version>
</dependency>
```

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

