+++
title = '008 Kotlin 中干点正经活：搜索一维函数最小值'
date = 2024-12-31T15:32:48+08:00
draft = false
mathkatex = true
categories = ['jetpack']
tags = ['jetpack', 'jenetics', 'kotlin', 'optimization']
toc = true
tocBorder = true
+++

## 正经干活

其实，我还是经常用Kotlin干正经事情的。以前也用一些Java，现在用上Kotlin之后，Java顿时就不香了。特别是用了amper之后，项目的干净程度又上升一个台阶。不用再写什么`build.gradle.kts`了，直接用`amper`的DSL就可以了。


## 问题

这一次，来展示一下用解决一个简单的问题：搜索一维函数的最小值。


### 目标函数
就随便写一个函数：

$$
f(x) = \cos (0.5 + \sin x) \cos x, x \in [0, 2\pi]
$$

实现为Kotlin代码：

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/Objects.kt" %}}
```

![](/jetpack-imgs/jenetics/output.png)


### 极小值条件

这个函数的最小值从图形上很容易看到，实在是太简单了。并且，从数学中我们可以得到最小值所在位置 $x^\*$ 满足：

$$
\begin{cases}
f'(x^\*) = 0 \\\\
f''(x^\*) > 0
\end{cases}
$$

从上面的图中可以看到，极值点有两个，最小值的点有一个。按照导数和二次导数的关系，也能通过网格搜索找到最小值的位置。

### 网格生成

要实现网格搜索，首先我们定义一个生成线性网络的函数，给定区间和点数，生成一个线性的网络，表达为`Array<Double>`。

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/Linspace.kt" %}} 
```

这个要这么设计而不是通过步长来产生主要是为了偷懒，如果设置步长的话，就必须处理步长不能整除的情况，要搞半天。反过来就简单多了。

### 导数计算

当然，要按照前面的极小值条件来找到最小值，就需要计算导数。这里我们用数值方法来计算导数，这样就不用去解析求导了。

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/Derive.kt" %}}
```

我们用了一个高精度方法来计算导数和二阶导数，这样步长的选择就可以稍微简单一点。高阶的方法有个代价，就是需要计算更多的目标函数，这里需要计算9次才能得到导数和二阶导数。

有了这两个函数，就想着实现一个设定步长，一次性表达 $x, f(x), f'(x), f''(x)$ 的方式，在Kotlin这样的高糖语言中，都不是事。

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/FunctionPointAndDerivatives.kt" %}}
```

这个函数有两个好玩的，一个就是采用了`data class`；另外一个就是`companion object`，这个是Kotlin的一个特性，可以在类中定义一个伴生对象，这个对象的方法和属性可以直接通过类名访问，就像Java中的静态方法一样。

一般而言，我们就可以这样来调用：

```kotlin
FunctionPointAndDerivatives.apply { h = 0.01}.of(0.0){x -> cos(0.5 + sin(x)) * cos(x)}
```
这样的调用方式，就相当完美了。

我还很无聊写了几个测试。

```kotlin
{{% codesnap "static/jetpack-code/jenetics/test/DeriveTest.kt" %}}
```

通过测试，可以看到，只需要步长采取1e-3，就能得到所有的两阶导数1e-9的精度，这在一般的计算中完全是足够的。


### 函数调用计数

当然，在最终来整网格搜索之前，我们还有一个需要考虑的问题，那就是统计函数调用次数。这个在优化算法中是一个很重要的指标，因为函数调用次数是一个很大的开销。我们可以通过一个奇怪的语法糖来实现它！

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/FunctionEvaluation.kt" %}}
```

首先，这是一个可以当作函数来调用的类，构造这个类的时候，需要传入一个函数，`T -> R`，这个函数就是我们要优化的目标函数。然后，我们可以通过`invoke`来调用这个函数，这个函数会返回一个`R`，也就是函数的值。同时，这个类还有一个`evaluations`属性，用来统计函数调用次数。

调用这个类的时候，就可以这样：

```kotlin
val f = FunctionEvaluation { x: Double -> cos(0.5 + sin(x)) * cos(x) }
val y = f(0.0)
println(f.evaluations)
```

这样就可以得到函数调用次数了。当然，那个`reset`方法是用来重置函数调用次数的。

### 网格搜索

在上面的条件下，就可以实现一个比较简单的网格搜索：

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/GridSearch.kt" %}}
```

这个函数的实现就是一个典型的函数式程序，首先产生一个网格（线性），调用`map`函数，变成一个 $x, f(x), f'(x), f''(x)$ 的列表，然后再调用`filter`函数，找到满足 $f''(x) > 0$ 的点，然后再调用`minBy`函数，找到一个 $f'(x)$ 最小的点。

这个逻辑实际上非常牵强。用不着这么麻烦，直接`minBy`找一个 $f(x)$ 最小的点就可以了。这里只是为了展示一下函数式编程的魅力。所以，你们也很容易看到，函数式编程通常会搞一些没有啥用的东西，把事情搞复杂，然后得到一个非常无聊的结果，如果热衷于函数式编程但是时时刻刻提醒自己这一点，就非常棒了。

我们如果设置网格点个数为5000， 则需要计算目标函数50000次（每次都要1+9）。最终得到一个最小值点为：

```
FunctionPointAndDerivatives(point=3.3885712318776084, fitness=-0.9381715901908968, firstDerivative=-0.0011090453000406342, secondDerivative=1.9998817416914485)
```

大概就是这个样子。

## 进化计算

### 遗传算法库
但是呢，采用了Amper之后，有一个事情变得非常简单，就是引入第三方库。这里我们引入一个遗传算法的库[Jenetics](https://jenetics.io/)，这个库是一个Java的遗传算法库，但是可以很好的和Kotlin一起使用。

首先，我们需要引入这个库：

```yaml
{{% codesnap "static/jetpack-code/jenetics/module.yaml" %}}
```

然后，我们就可以开始使用这个库了。

### 流式API

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/GeneticExample.kt" %}}
```

这个库的效果非常炸裂，我在工程中经常用，来用这个做过一个遗传编程来拟合函数表达式的软件。这个库的特点就是流式API，熟悉高版本Java的应该非常熟悉。这个库的文档也非常好，值得一看。

这里就不详细介绍了。

### 输出图形

前面，我们还做了一个函数及其导数的图像，看起来很戳，但其实都是我的回忆。采用的是上古图形库`JFreeChart`，我唯一的目的就是看看这个库还能不能用。这个库的文档也是非常好的，但是我已经很久没有用了。

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/CreateDataset.kt" %}}
```

上面就是准备数据集合来画图。下面就是画图并输出的代码。

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/ExportPng.kt" %}}
```

啊，我的青春……

## 总结

这个例子的主函数：

```kotlin
{{% codesnap "static/jetpack-code/jenetics/src/Main.kt" %}}
```

我们的Jenetics只用了273次函数求值（实际上还可以更少）就找到了最小值点：

```
Minimum found by Jenetics: [[[3.388506909464689]]] -> -0.9381715147169912
Number of evaluations: 273
```

这个结果很好地证明了遗传算法能够取得非常大的收益。

整个项目的代码可以下载：

- [jenetics.zip](/jetpack-code/jenetics/jenetics-example.zip)

下载解压后，可以直接用`IntelliJ IDEA`打开，然后运行`Main.kt`就可以看到结果了。

或者Gradle和Amper都可以直接使用，我都已经设置好镜像服务器，也就是Amper本身的下载会稍微慢一点点。

```shell
./gradlew.bat run
```

或者

```shell   
./amper.bat run
```

这个例子就到这里了，希望大家喜欢。