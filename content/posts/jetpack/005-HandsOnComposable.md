+++
title = 'Jetpack Compose for Desktop-005 程序猿初试Composable'
date = 2024-11-04T20:21:31+08:00
draft = false
mathjax = false
categories = ['jetpack']
tags = ['jetpack']
toc = true
tocBorder = true
+++


## 组合的概念
在Jetpack Compose中，提出了一个概念就是可组合的声明式界面开发。

这个Compose就是可组合中“组合”的意思。也就是我们描述界面，有一系列可以组合使用的元素，这些元素嵌套组合，构成复杂的界面。这种方式和传统的XML方式有点类似。

比如，描述一列标签构成的界面，我们可以这样写：

```kotlin
@Composable
fun Greeting(name: String) {
    Text(text = "Hello $name!")
}
```

这个例子很烦人，每个教程的开头都是这个例子，但是这个例子很好地展示了Compose的特点。我们通过一个函数来描述一个标签，这个标签的内容是一个字符串，这个字符串是一个模板，我们可以通过模板来生成不同的标签。这个函数就是一个可组合的函数，我们可以在其他地方调用这个函数，来生成一个标签。

乃至，我们在描述一个按钮的时候，也是类似的：

```kotlin
@Composable
fun MyButton(text: String, onClick: () -> Unit) {
    Button(onClick = onClick) {
        Text(text = text)
    }
}
```

可以看到，这里按钮中的文字，居然是一个标签，

```Java
JButton button = new JButton("Click Me");

button.addActionListener(new ActionListener() {
    @Override
    public void actionPerformed(ActionEvent e) {
        System.out.println("Button Clicked!");
    }
});
```

这跟传统Java Swing的方式不同，Java Swing的按钮是一个对象，这个对象有一个方法叫做 `setText`，我们可以通过这个方法来设置按钮的文字。而在Compose中，按钮的文字是一个标签，这个标签是一个函数，我们可以通过这个函数来设置按钮的文字。听起来也没啥不同的。

但是这里实际上涉及到一个功能分离和复用的概念。按钮的核心是点击事件；文本控件关心的是显示一串字符。当已经有了文本控件，按钮只需要内涵（组合）一个文本控件，而不需要自己在额外处理文本、字符串的显示。这样的设计，使得我们可以更加灵活地组合界面元素，而不需要关心界面元素的具体实现。

这也是可组合的核心思想，不同的功能，组合起来使用，而不是形成复杂的对象继承关系，难以追溯某个具体的功能是在哪个类中实现的。

例如，Compose中，我们的布局，也是通过可组合的函数来实现的：

```kotlin
@Composable
inline fun Column(
    modifier: Modifier = Modifier,
    verticalArrangement: Arrangement.Vertical = Arrangement.Top,
    horizontalAlignment: Alignment.Horizontal = Alignment.Start,
    content: @Composable ColumnScope.() -> Unit
) {
    val measurePolicy = columnMeasurePolicy(verticalArrangement, horizontalAlignment)
    Layout(
        content = { ColumnScopeInstance.content() },
        measurePolicy = measurePolicy,
        modifier = modifier
    )
}
```

这个可组合函数实现了一个竖向配列其内容的布局。可以看看这个函数的几个参数，来猜一猜这个函数本身的功能。

- `modifier`：布局的修饰器，用来修饰布局的样式。
- `verticalArrangement`：垂直排列的方式，可以是顶部对齐、居中对齐、底部对齐。
- `horizontalAlignment`：水平对齐的方式，可以是左对齐、居中对齐、右对齐。

最后一个参数，正好是接受者函数，这个函数的功能是描述布局的内容。这个函数的接受者是一个 `ColumnScope`，这个 `ColumnScope` 是一个接口，用来描述布局的内容。这个接口中包含了一系列的函数，用来描述布局的内容。

我们就能在这个函数中，描述布局的内容，比如：

```kotlin
Column {
    Text("Hello, World!")
    Text("Hello, Compose!")
    Greeting("Android")
    Row {
        Text("Hello, World!")
        Text("Hello, Compose!")
        Greeting("Android")
    }
}
```

看看，嵌套一个自定义的`Compose`函数，这就是可组合的思想。我们可以通过这种方式，组合各种各样的界面元素，来构建复杂的界面。

核心的概念：

- 一个可组合函数描述一项能力（功能）
- 自由组合无负担
- 实现机制：函数接受者
- 定义方法：`@Composable`注解

## 对照实际的界面开发

![tic-tac-toe](/jetpack-imgs/tictactoe.gif)

### 卑微的起点

这个井字棋的界面，我们在需求分析的阶段可能提出如下的描述：

- 3x3的格子，可以放置棋子
- 棋子有两种状态，X和O
- 点击格子，可以放置棋子
- 每轮切换棋子状态
- 提供一个展示当前玩家的标签、同时显示胜负

我们首先可以自己定义一个`Grid`函数，试试看：

```kotlin
@Composable
fun Grid() {
    Column {
        Row {
            Cell()
            Cell()
            Cell()
        }
        Row {
            Cell()
            Cell()
            Cell()
        }
        Row {
            Cell()
            Cell()
            Cell()
        }
    }
}
```

如果这个`Cell`函数是一个可组合函数，描述一个格子的话，那么这个`Grid`函数就是描述一个3x3的格子的布局。这样，我们就完成了一个简单的界面的描述。

对于整个界面，我们可以这样描述：

```kotlin
@Composable
fun TicTacToe() {
    Column {
        Grid()
            Text("Restart")
        }
        Text("Player X's turn")
    }
}
```

这样，我们就完成了一个简单的界面的描述。

### 稍微好一点点的改进

看看我们前面的`Grid`函数，正常情况下，我们就会试图通过循环来生成这个3x3的格子。这样，我们就可以通过一个循环来生成这个3x3的格子，而不是一个一个手动写。

```kotlin
@Composable
fun Grid() {
    Column {
        for (i in 0..2) {
            Row {
                for (j in 0..2) {
                    Cell()
                }
            }
        }
    }
}
```

这样是不是就显得高级多了，也让可组合函数的内涵更加丰富，组合函数，甚至可以让程序来组合。

### 游戏中的最终界面

最终我们的选择跟上面类似，就是增加了亿点点细节。

首先是主函数：

```kotlin
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

我们忽略前面的配置加载，直接看`Window`函数，这个函数是一个窗口的描述，我们可以看到，这个窗口的内容是一个`Box`，这个`Box`是一个盒子布局，这个盒子布局的内容是一个`TicToc`函数。

这里，我们就把`Window`理解为一个窗口，这个窗口的参数大概如下：

```kotlin
@Composable
fun Window(
    onCloseRequest: () -> Unit,
    state: WindowState = rememberWindowState(),
    visible: Boolean = true,
    title: String = "Untitled",
    icon: Painter? = null,
    undecorated: Boolean = false,
    transparent: Boolean = false,
    resizable: Boolean = true,
    enabled: Boolean = true,
    focusable: Boolean = true,
    alwaysOnTop: Boolean = false,
    onPreviewKeyEvent: (KeyEvent) -> Boolean = { false },
    onKeyEvent: (KeyEvent) -> Boolean = { false },
    content: @Composable FrameWindowScope.() -> Unit
) {
//    ...
}
```
在实际的调用中，我们可以什么参数都不用设置，只需要利用最后那个`content: @Composable FrameWindowScope.() -> Unit`参数，来描述窗口的内容。

当然，我们这里增加了三个参数，`onCloseRequest`是窗口关闭的回调函数，`state`是窗口的状态，`title`是窗口的标题。可以看到，如何设定窗口的位置和大小，都是通过`state`参数来设置的。这个参数的玄机，还得下一次再讲。

现在看`Window`组合的唯一一个对象， `Box`，这个`Box`是一个盒子布局，其内容是一个`TicToc`函数。

```kotlin
Box(
    modifier = Modifier.background(Color.White).fillMaxSize(),
    contentAlignment = Alignment.Center
) {
    TicToc()
}
```

这里`Box`的第一个参数是一个典型的组合对象修饰方法，大部分（几乎所有）的组合对象都有这个修饰方法，用来修饰这个组合对象的样式。这里，我们设置了背景颜色为白色，然后设置了这个盒子布局的大小为最大。然后这个`Modifier`最好的特点就是可以链式调用，这样我们可以一次性设置多个修饰条件。第二个函数式参数是一个对齐方式，这个对齐方式是用来设置这个盒子布局的内容的对齐方式。

在`TicToc`函数中，我们可以描述整个井字棋的界面：

```kotlin
@Composable
@Preview
fun TicToc() {
    // ...

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
```

好吧，这跟前面我们设计的完全相同，不过也是增加了亿点点细节，修饰符啊，记录的信息啊。

值得注意的是，这里的标签同样增加了修饰符，`Modifier`对象。具有很好的一致性。

那么在这个`TicTocTile`函数中，我们可以描述一个格子的内容：

```kotlin
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
```

更多的细节，毫无必要的复杂性……我们同样是一个`Box`，里面组合一个`Button`，这个`Button`里面是一个`Text`。

无穷无尽的组合，无穷无尽的修饰符，这就是Compose的世界。

这里也就是设置了一些颜色啊、边界啊、间隔啊、大小啊。最后，通过一个`onClick`事件，来处理点击事件。这个当然也要下面再详细说明。

从前面的概念和具体的例子，我们已经对Compose的可组合的声明式界面开发有了一个初步的感受了。

这里的核心思想就是：

- 一个可组合函数描述一项能力（功能）
- 自由组合
- 实现机制：函数接受者
- 增加修饰符，逐步完善

## Jetpack Compose提供的常用布局和控件

作为一个不那么成熟的桌面开发工具，Jetpack Compose Desktop提供的布局和控件还是比较有限的。

### 标准布局

- `Column`：竖向布局
- `Row`：横向布局
- `Box`：盒子布局

![basic-layout](/jetpack-imgs/layout-column-row-box.png)

通过嵌套这三种布局，设置各种不同的修饰条件，我们可以实现各种复杂的布局。

### 常用控件

- `Text`：文本控件
- `Button`：按钮控件
- `TextField`：文本输入框
- `Checkbox`：复选框
- `RadioButton`：单选按钮
- `Slider`：滑动条
- `ProgressBar`：进度条

总之，Jetpack Compose Desktop提供了一系列的基确布局和控件，可以满足基本的界面开发需求。

## 总结

设计了UI，布局了界面，接下来就是处理事件和交互：

- 输入信息
- 显示信息

这两个核心的界面开发内容，下一次我们再来讨论。