+++
title = 'Jetpack Compose for Desktop-004 快速开发井字棋'
date = 2024-11-04T20:19:01+08:00
draft = false
mathjax = false
categories = ['jetpack']
tags = ['jetpack', 'compose-desktop', 'kotlin']
toc = true
tocBorder = true
+++


## 快速入水

要学习一样跟程序设计有关的东西，最好的办法始终是把手打湿，整一个能够运行，可以实验的东西出来。

也只有在程序开发中，我们才能想一个魔法师而不是魔术师，我们真的能够创造一个东西。而且编译器不会因为我们很丑、我们学历不行、我们没女朋友、我们很穷而拒绝我们，只要我们严格按照手册，就真的可以梦想成真。真是幸运啊！

所以，我们先来写一个简单的程序，一个大家都完全不会感兴趣的游戏：井字棋。

这个游戏实在是无聊，我从来没有成功说服任何一个人跟我一起好好玩过……不过……无聊也是它的好处，马上就行。

## 构建工具链

现代化的程序设计语言通常都有完整的工具链，负责完成：

- 管理工程-文件的组织
- 编译源代码
- 运行、运行、分发程序

热门但是无法流行的伟大Rust语言，它的工具链是`cargo`。

不热门但是很流行的C/C++语言，它的工具链是`make`，现在也有`cmake`，或者`ninja`。

热门流行的Java语言，它的工具链是`ant`、`maven`和`gradle`。

Kotlin是基于Java的，它的工具链我们一般选择`gradle`。实际上`gradle`、`maven`、`ant`都是Java的构建工具。

用`gradle`构建Kotlin程序，最好玩的是，我们可以编写`kotlin`来完成。当然，以前还用`groovy`，现在大概可能或许提倡用`kotlin`。

构成一个`gradle`工程的文件有：

- `settings.gradle.kts`：工程的设置
- `build.gradle.kts`：工程的构建
- `src`：源代码目录
- `gradle.properties`：gradle的属性文件
- `gradlew`和`gradlew.bat`：gradle的wrapper的启动脚本
- `gradle/wrapper`目录：gradle的配置文件和wrapper
  - `gradle-wrapper.jar`
  - `gradle-wrapper.properties`

这是在IDEA中一个典型工程的结构：

![IDEA工程结构](/jetpack-imgs/project-structure.png)

这里展示了全部的文件，包括隐藏文件:

- `.gradle`：gradle的缓存目录
- `.idea`：IDEA的配置目录
- `build`：gradle的构建目录
- `.kotlin`：kotlin的配置目录
- `.run`：运行配置目录

有些时候，还有：

- `out`：编译输出目录

当然，这些都不需要自己来创建，只需要在IDEA中创建一个新的`gradle`工程就行了。甚至，下面大部分配置都不需要自己来创建，只需要安装一个Jetpack Compose的插件，然后新建一个Compose工程就行了。

但是，我们这里还是絮絮叨叨一下，可以大概知道这些都是什么。

### Gradle Wrapper

为了让工程更加有移植性，我们一般会使用`gradle wrapper`，这个工具会自动下载指定版本的`gradle`，并且把`gradle`的启动脚本放在工程的根目录下。

我们可以通过`gradlew`或者`gradlew.bat`来启动`gradle`，这样就不需要在系统中安装`gradle`了。

这相关的几个文件，我们可以通过`gradle wrapper`命令来生成。

```shell
gradle wrapper
```

这个命令会生成`gradlew`、`gradlew.bat`、`gradle/wrapper/gradle-wrapper.jar`、`gradle/wrapper/gradle-wrapper.properties`这几个文件。

在最后那个文件中，通常包含有下载的地址和版本号。

```gradle-wrapper.properties
distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
distributionUrl=https\://mirrors.cloud.tencent.com/gradle/gradle-8.7-bin.zip
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
```

我们通（bi）常（xu）更改那个`distributionUrl`地址，指向我们自己的镜像源，这样下载会快一些。


### `gradle.properties`

这个文件通常用来存放一些工程的属性，比如版本号、插件版本等等。

```properties
org.gradle.jvmargs=-Xmx2048m -Dfile.encoding=UTF-8
kotlin.code.style=official
kotlin.version=2.0.0
compose.version=1.6.10
```

第一行的`org.gradle.jvmargs`是`gradle`的启动参数，这里设置了堆内存和文件编码。

在调用`gradle`的时候，我们可以通过`-P`参数来传递这些属性，比如：

```shell
gradle build -Pkotlin.version=1.5.31
```

这里设定的属性，可以在`build.gradle.kts`文件中使用。

### `settings.gradle.kts`

我们需要在`settings.gradle.kts`文件中添加一些内容，这个文件是一个Kotlin脚本，用来描述工程的设置。

```kotlin
pluginManagement {
    repositories {
        maven("https://maven.aliyun.com/repository/public")
        maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
        google()
        gradlePluginPortal()
        mavenCentral()
    }

    plugins {
        kotlin("jvm").version(extra["kotlin.version"] as String)
        id("org.jetbrains.compose").version(extra["compose.version"] as String)
        id("org.jetbrains.kotlin.plugin.compose").version(extra["kotlin.version"] as String)
    }
}

rootProject.name = "Demo004"
```

这个文件首先描述了插件的管理，然后指定了工程的名称。

当然，我们也在这里添加了一些镜像源，这样下载插件会快一些。这个技能必须要掌握……不掌握就会觉得体验极其糟糕……什么都打不开，什么都运行不了。



### `build.gradle.kts`

我们需要在`build.gradle.kts`文件中添加一些内容，这个文件是一个Kotlin脚本，用来描述工程的构建过程。

```kotlin
import org.jetbrains.compose.desktop.application.dsl.TargetFormat
import org.jetbrains.kotlin.gradle.targets.js.npm.fromSrcPackageJson

plugins {
    kotlin("jvm")
    id("org.jetbrains.compose")
    id("org.jetbrains.kotlin.plugin.compose")
}

group = "org.cardc.fdii"
version = "1.0.0"

repositories {
    maven("https://maven.aliyun.com/repository/public")
    mavenCentral()
    maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
    google()
}

dependencies {
    // Note, if you develop a library, you should use compose.desktop.common.
    // compose.desktop.currentOs should be used in launcher-sourceSet
    // (in a separate module for demo project and in testMain).
    // With compose.desktop.common you will also lose @Preview functionality
    implementation(compose.desktop.currentOs)

    // Gson dependency
    implementation("com.google.code.gson:gson:2.11.0")
}

compose.desktop {
    application {
        mainClass = "MainKt"

        nativeDistributions {
            targetFormats(TargetFormat.Dmg, TargetFormat.Msi, TargetFormat.Deb)
            packageName = "Demo004"
            packageVersion = "1.0.0"
        }
    }
}
```

这里面，通常只更改了几处：

- `group`：工程的组织
- `version`：工程的版本，通常我会从`1.0-SNAPSHOT`改成`1.0.0`，前者在分发的时候会有问题
- `repositories`：镜像源，这里添加了一些镜像源，同样！这个技能必须要掌握！
- `dependencies`：依赖，这里`compose.desktop.currentOs`，这是Compose Desktop的依赖，如果是Compose工程，自动就有；后面的`gson`是一个JSON库，我们可能会用到。

### `src`目录

这个目录是源代码目录，我们可以在这个目录下创建`kotlin`、`java`、`resources`等等目录，用来存放源代码和资源文件。

当然，以这个工程为例，我们只需要在`src/main/kotlin`目录下创建一个`Main.kt`文件，这个文件就是程序的入口。

```kotlin
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.Button
import androidx.compose.material.ButtonDefaults
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.useResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.compose.ui.window.Window
import androidx.compose.ui.window.WindowPosition
import androidx.compose.ui.window.application
import androidx.compose.ui.window.rememberWindowState
import java.io.File

@Composable
fun TicTocTile(
    x: Int,
    y: Int,
    ticTacToe: TicTacToe
) {

    Box(
        modifier = Modifier
            .size(100.dp)
            .background(Color.LightGray),
        contentAlignment = Alignment.Center
    ) {

        Button(modifier = Modifier
            .padding(5.dp)
            .fillMaxSize(),
            colors = ButtonDefaults.buttonColors(backgroundColor = ticTacToe[x, y].color()),
            onClick = {
                if (ticTacToe.isGameOver()) {
                    ticTacToe.startNewGame()
                    return@Button
                }

                ticTacToe.nextMove(x, y)

            }) {
            Text(
                text = ticTacToe.textAt(x, y),
                fontSize = 36.sp,
                color = Color.Black
            )
        }
    }
}


@Composable
@Preview
fun TicToc() {
    val board = remember {
        mutableStateMapOf<Pair<Int, Int>, Player>(
            Pair(0, 0) to Player.NULL,
            Pair(0, 1) to Player.NULL,
            Pair(0, 2) to Player.NULL,
            Pair(1, 0) to Player.NULL,
            Pair(1, 1) to Player.NULL,
            Pair(1, 2) to Player.NULL,
            Pair(2, 0) to Player.NULL,
            Pair(2, 1) to Player.NULL,
            Pair(2, 2) to Player.NULL
        )
    }
    val player = remember { mutableStateOf(Player.X) }

    val ticTacToe = TicTacToe(board, player)

    Column {
        for (i in 0..2) {
            Row {
                for (j in 0..2) {
                    TicTocTile(i, j, ticTacToe)
                }
            }
        }
        Text(
            modifier = Modifier.padding(10.dp).align(Alignment.CenterHorizontally),
            text = ticTacToe.gameText(),
            fontSize = 30.sp,
            color = ticTacToe.color(),
            textAlign = TextAlign.Center
        )

    }

}

fun main() = application {
    useResource("config.json") {
        config = loadConfig(it)
    }

    Window(
        onCloseRequest = ::exitApplication,
        title = "Tic Tac Toe",
        state = rememberWindowState(
            position = WindowPosition.Aligned(Alignment.Center),
            size = DpSize(320.dp, 400.dp)
        )
    ) {
        Box(
            modifier = Modifier.background(Color.White).fillMaxSize(),
            contentAlignment = Alignment.Center
        ) {
            TicToc()
        }

    }
}
```

然后是`TicTacToe`的实现：

```kotlin
enum class Player {
    NULL, X, O;

    fun nameString(): String {
        return when (this) {
            X -> "X"
            O -> "O"
            else -> ""
        }
    }


}

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.snapshots.SnapshotStateMap


data class TicTacToe(val board: SnapshotStateMap<Pair<Int, Int>, Player>, val player: MutableState<Player>) {
    fun gameText(): String {
        val winner = winner()
        if (isGameOver()) {
            if (winner == Player.NULL) {
                return "Game Over Tie."
            }
            return "Player ${winner.nameString()} won!"
        }
        return "Player ${player.value.nameString()}'s turn"
    }


    fun winner(): Player {
        for (i in 0..2) {
            if (get(i, 0) != Player.NULL && get(i, 0) == get(i, 1) && get(i, 1) == get(i, 2)) {
                return get(i, 0)
            }
            if (get(0, i) != Player.NULL && get(0, i) == get(1, i) && get(1, i) == get(2, i)) {
                return get(0, i)
            }
        }
        if (get(0, 0) != Player.NULL && get(0, 0) == get(1, 1) && get(1, 1) == get(2, 2)) {
            return get(0, 0)
        }
        if (get(0, 2) != Player.NULL && get(0, 2) == get(1, 1) && get(1, 1) == get(2, 0)) {
            return get(0, 2)
        }
        return Player.NULL
    }


    fun nextMove(x: Int, y: Int) {
        if (isTaking(x, y)) return
        board.put(Pair(x, y), player.value)
        nextPlayer()
    }

    operator fun get(x: Int, y: Int): Player {
        return board[Pair(x, y)] ?: Player.NULL
    }

    fun textAt(x: Int, y: Int): String {
        return get(x, y).nameString()
    }

    fun startNewGame() {
        for (key in board.keys) {
            this.board.put(key, Player.NULL)
        }
        player.value = Player.X
    }

    fun isGameOver(): Boolean {
        return winner() != Player.NULL || isFilled()
    }

    private fun isFilled(): Boolean {
        board.values.find { it == Player.NULL }?.let {
            return false
        }
        return true
    }

    private fun isTaking(x: Int, y: Int): Boolean {
        return get(x, y) != Player.NULL
    }

    private fun nextPlayer() {
        player.value = if (player.value == Player.X) Player.O else Player.X
    }


}
```

为了配合显示颜色，我们还需要一个扩展函数：

```kotlin
import Player.O
import Player.X
import androidx.compose.ui.graphics.Color
import com.google.gson.Gson
import java.io.File
import java.io.InputStream


data class Config(
    val PlayerXColor: String="00FF00",
    val PlayerOColor: String="#0000FF",
    val GameOverColor: String="##FF0000"
)

fun loadConfig(file: InputStream): Config {

    try {
        val json = file.readAllBytes().decodeToString()
        return Gson().fromJson(json, Config::class.java)
    } catch (e: Exception) {
        e.printStackTrace()
        return Config("#FF0000", "#0000FF", "#00FF00")
    }
}

val String.color
    get() = Color(removePrefix("#").toInt(16)).copy(alpha = 1f)

lateinit var config: Config

fun Player.color(): Color {
    return when (this) {
        X -> config.PlayerXColor.color
        O -> config.PlayerOColor.color
        else -> Color.White
    }
}

fun TicTacToe.color(): Color {
    if (isGameOver()) {
        return config.GameOverColor.color
    }
    return player.value.color()
}
```

这个负责从`config.json`文件中读取颜色配置，然后在游戏中对不同玩家显示不同颜色，并给出游戏结束时的颜色。

这个`config.json`文件的内容是：

```json
{
  "PlayerXColor": "#00FF00",
  "PlayerOColor": "#0000FF",
  "GameOverColor": "#FF0000"
}
```

这样，我们就完成了一个简单的井字棋游戏。

## 运行

在windows下面，我们可以通过`gradlew.bat`来运行这个程序：

```shell
./gradlew.bat run
```

还能够通过`gradle`来构建这个程序：

```shell
./gradlew.bat createDistributable
```

这样就会在`build/compose/binaries`目录下生成一个可执行文件程序文件夹，里面包含了可执行文件和依赖库。

这个文件拷贝到其他地方，就可以运行了。

当然在IDEA中——默认你使用的是IDEA——你可以直接右边的`Gradle`工具栏中选择`Tasks`，然后选择`application`，然后选择`run`，就可以运行这个程序。


![play](/jetpack-imgs/tictactoe.gif)

## 总结

这个程序是一个简单的井字棋游戏，我们通过Jetpack Compose来实现了界面，通过Kotlin来实现了逻辑。

至于程序的实现细节，源代码的解读，就放在下次。

