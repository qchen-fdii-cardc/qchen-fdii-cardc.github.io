pluginManagement {
  repositories {
    maven("https://maven.aliyun.com/repository/public/")
    maven("https://repo.huaweicloud.com/repository/maven/")
    maven("https://mirrors.cloud.tencent.com/nexus/repository/maven-public/")
    mavenCentral()
    google()
    gradlePluginPortal()
    maven("https://packages.jetbrains.team/maven/p/amper/amper")
    maven("https://www.jetbrains.com/intellij-repository/releases")
    maven("https://packages.jetbrains.team/maven/p/ij/intellij-dependencies")
  }
}

plugins {
  id("org.jetbrains.amper.settings.plugin").version("0.5.0")
}