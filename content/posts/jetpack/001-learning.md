+++
title = 'Jetpack Compose for Desktop-001 学习Compose Desktop'
date = 2024-11-04T20:11:42+08:00
draft = false
mathjax = false
categories = ['jetpack']
tags = ['jetpack', 'compose-desktop', 'kotlin']
toc = false
tocBorder = false
+++


# 学习Compose Desktop

## 学习过程

### 理解Kotlin的基本语法
Compose Desktop采用Kotlin构建，因此对Kotlin的基本语法有很好的理解是必不可少的。你可以从官方的[Kotlin文档](https://kotlinlang.org/docs/home.html)开始。

用一句话概括，Kotlin是一种现代的、静态类型的编程语言，它结合了面向对象和函数式编程的特性。用通俗的话说，Kotlin就是Java的含糖量爆表的全新版本。它非常现代，也非常简洁，同时还保留了Java的一些优点。

学习Kotlin的基本语法，包括变量、函数、类、接口、扩展函数、数据类、枚举类等，是学习Compose Desktop的基础。但是一个Java程序员，应该很快就能掌握Kotlin的基本语法。并且，JetBrains提供了一个非常好的[在线Kotlin Playground](https://play.kotlinlang.org/)，你可以在这里练习Kotlin的基本语法。JetBrains的免费Java开发环境IntelliJ IDEA也支持Kotlin，你可以在IntelliJ IDEA中创建一个Kotlin项目，并且支持把Java代码转换为Kotlin代码。

糖度高、身体好，Kotlin是Java程序员的福音。

###  开发环境搭建

- **安装IntelliJ IDEA**：这是Kotlin和Compose Desktop开发的推荐IDE。
- **安装JDK**：确保你安装了最新的JDK。
- **创建一个新项目**：使用IntelliJ IDEA创建一个带有Compose Desktop支持的Kotlin项目。

这几个步骤就已经完成了Compose Desktop的开发环境搭建。

### 学习Jetpack Compose的基础知识

Jetpack Compose是Compose Desktop的基础。从官方的[Jetpack Compose文档](https://developer.android.com/jetpack/compose/documentation)开始，了解核心概念。

这个部分主要包括一些非常基础的概念，如：

- Composable函数的概念
- GUI界面的概念
- Material Design的概念
- 界面布局的概念
- 基本的UI组件，如Button、TextField、Checkbox等



### 探索多平台Compose

- **官方文档**：参考[Compose Multiplatform文档](https://github.com/JetBrains/compose-multiplatform).
- **示例项目**：在GitHub上探索可用的示例项目，看看Compose Desktop在实际应用中是如何使用的。



### Compose Desktop应用示例

首先，我们来看一个最简单的Compose Desktop应用示例。这个示例展示了如何创建一个简单的窗口，并在窗口中显示一段文本。


```kotlin
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.Window
import androidx.compose.ui.window.application
import java.awt.Dimension

@Composable
@Preview
fun App() {
    MaterialTheme {
        Box(
            modifier = Modifier.fillMaxSize(),
            contentAlignment = Alignment.Center
        ) {
            Text("您好, Compose Desktop!", style = MaterialTheme.typography.h3)
        }
    }
}

fun main() = application {
    Window(
        onCloseRequest = ::exitApplication,
        title = "Compose Desktop Demo 000"
    ) {
        App()
    }
}

```

这个代码，跟JavaFx的实际上比较类似，JavaFx中的容器、布局类、空间类，在Compose Desktop中都有对应的概念，只是Compose Desktop采用可组合函数的方式来实现。


而在编译这个代码之前，最好先把Gradle的下载地址换个国内的镜像，就在`gradle/wrapper/gradle-wrapper.properties`文件中，把`distributionUrl`的地址换成国内的镜像地址，比如`https\://services.gradle.org/distributions/gradle-8.7-bin.zip`，更换为`distributionUrl=https\://mirrors.cloud.tencent.com/gradle/gradle-8.7-bin.zip`。这样下载速度会快很多。

另外就是在`build.gradle.kts`文件中，添加国内的maven仓库地址，如下：

```kotlin
repositories {
    maven(url = "https://maven.aliyun.com/repository/public")
    mavenCentral()
}
```

这两个步骤完成之后，就可以编译这个Compose Desktop应用程序了。


![](/jetpack-imgs/000-gui.png)

可以调用`gradle run`命令来运行这个应用程序。

还能用`gradle createRuntimeImage`命令来创建一个可执行文件，这个可执行文件可以在没有安装JDK的机器上运行。

大概在`build/compose/binaries/app/Demo000`目录下，有一个可执行文件，双击这个文件就可以运行这个应用程序。整个目录大概170MB左右。当然，这都不重要……


### 理解状态管理

学会如何在Compose Desktop中管理状态。这对于构建交互式应用程序至关重要。这个地方实际上是Compose Desktop的核心，也是最难的地方。

Compose Desktop的核心思想是函数式编程，状态管理是函数式编程的核心。Compose Desktop提供了一些函数来帮助我们管理状态，如`remember`和`mutableStateOf`。这些函数可以帮助我们在Compose Desktop中管理状态，使得我们的应用程序更加健壮和易于维护。

### 探索布局和修饰符

布局是一切GUI设计的中心工作之一。

理解不同的布局组合（`Box`、`Column`、`Row`等）以及如何使用`Modifier`来设计和定位UI元素，是构造Compose Desktop应用程序的主要工作内容。


### 用户输入的处理

学会如何使用`Button`、`TextField`和`Checkbox`等组合来处理用户输入。

一方面要掌握工具，也就是具体的用于处理用户输入的组合函数，另一方面也要仔细学习不同组件的实际用途，跟需求分析结合在一起。


### 列表

显示数量不定的数据是GUI设计的常见需求。Compose Desktop提供了`LazyColumn`和`LazyRow`等组合函数来处理这种情况。


### 高级主题

- **自定义绘图**：学会如何使用`Canvas`组合来创建自定义绘图。
- **动画**：探索如何为Compose Desktop应用程序添加动画。
- **互操作性**：了解如何与现有的Swing应用程序进行互操作。



### 测试

学会为Compose Desktop应用程序编写测试，以确保它按预期工作。

Compose Desktop提供了一些测试工具，如`ComposeTestRule`和`onNode`，可以帮助我们编写测试。这些工具可以帮助我们确保我们的应用程序按预期工作，同时也可以帮助我们快速定位和修复问题。


### 应用部署

学会如何使用`compose.desktop.application`插件打包和分发Compose Desktop应用程序。



## 总结

Compose Desktop是一个非常强大的桌面应用程序框架，它提供了一种现代、简洁、易于使用的方式来构建桌面应用程序。学习Compose Desktop需要掌握Kotlin的基本语法、Jetpack Compose的基础知识、状态管理、布局和修饰符、用户输入的处理、列表、高级主题、测试和应用部署等内容。

通过上面的学习过程，你将获得对Compose Desktop的全面理解，并能够构建自己的桌面应用程序。