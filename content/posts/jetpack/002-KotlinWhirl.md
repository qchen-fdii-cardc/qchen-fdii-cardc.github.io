+++
title = 'Jetpack Compose for Desktop-002 Kotlin旋风之旅'
date = 2024-11-04T20:15:42+08:00
draft = false
mathjax = false
categories = ['jetpack']
tags = ['jetpack', 'compose-desktop', 'kotlin']
toc = true
tocBorder = true
+++


## Compose Desktop中哪些Kotlin知识是必须的？

在学习Compose Desktop中，以下Kotlin知识是必须的：

- 基础语法：包括变量声明、数据类型、条件语句、循环等。
- 面向对象编程：类与对象、继承、接口、抽象类等。
- 函数式编程：高阶函数、Lambda表达式、内联函数等。
- 协程：理解协程的基本概念、使用launch和async等构建并发程序。
- Kotlin标准库：熟悉常用的集合操作、字符串处理、文件I/O等。
- DSL（领域特定语言）：Compose本身就是一个DSL，理解如何构建和使用DSL非常重要。
- Kotlin扩展函数：如何为现有类添加新功能。
- Kotlin特性：如空安全、数据类、解构声明等。
- 掌握这些知识将有助于你更好地使用Compose Desktop进行开发。

## 基础语法

### 变量与类型


在Kotlin中，使用`val`和`var`关键字来声明变量。`val`声明的变量是只读的，一旦赋值就不能再修改；`var`声明的变量是可变的。

从下面的例子可以看到，1）变量类型后置；2）没有分号！

```kotlin
val name: String = "Alice"
val age: Int = 18
val isStudent: Boolean = true
var score: Double = 99.5
```

并且，Kotlin中的变量类型可以省略，编译器会根据赋值的类型自动推断变量的类型。

```kotlin
val name: String = "Alice"
val age: Int = 18
val isStudent: Boolean = true
var score: Double = 99.5
```

在Java的基础上，Kotlin引入了一些新的数据类型，如`String`、`Int`、`Boolean`等。这些数据类型是不可变的，也就是说，一旦创建，就不能再修改。总的来说，Kotlin的基础数据类型某些时候比Java的更好用，提供了更多的功能。Kotlin的类与Java的类可以直接互操作，这是因为Kotlin是在Java虚拟机上运行的。

通过变量的`javaClass`属性，可以获取变量的Java类。

```kotlin
val name: String = "Alice"
println("name is a Java Class: ${name.javaClass}") // class java.lang.String
```

这里还可以看到，Kotlin的字符串是Java的`String`类，同时，Kotlin支持字符串模板，可以在字符串中插入变量。


对于变量和类型，Kotlin最甜的就是增加了扩展方法，这是Java中没有的特性。扩展方法可以为现有的类添加新的方法，而不需要继承这个类。这样，我们可以为Java的类添加新的方法，而不需要修改Java的源代码。

```kotlin
fun String.addHello(): String {
    return "Hello, $this"
}

val name: String = "Alice"
println(name.addHello()) // Hello, Alice

```

Kotlin标准库和基础类型中有很多这样的扩展方法，可以方便地操作字符串、集合等。同时，也可以定义扩展属性，因为属性本身就只是两个方法的语法糖（Java就没有~~~）。


扩展方法在Jetpack Compose中非常常见，非常常用。🙋‍♀️🌰，描述尺寸时，常常会使用一个单位`Dp`，那么调用的时候通常会有

```kotlin
val size: Dp = 16.dp
```

后面这个语法就特别奇怪，居然调用整数的`dp`属性，简直翻天了。在源程序中，我们可以看到：


```kotlin
@Stable
inline val Int.dp: Dp get() = Dp(value = this.toFloat())
```

这是一个只读的属性（只有`get`方法），返回的是一个`Dp`对象。这个`Dp`对象是一个数据类，包含一个`value`属性，表示尺寸的值。这样，我们就可以直接使用`Int`的`dp`属性来创建一个`Dp`对象，而不需要调用`Dp`的构造函数。真是完美的语法糖。


### 控制语句

在Java的基础上，Kotlin引入了一些新的控制语句，如`when`表达式、`if`表达式等。这些新的控制语句使得代码更加简洁、易读。

`when`表达式是Kotlin中的一个强大的控制语句，可以替代Java中的`switch`语句。`when`表达式可以匹配任意类型的值，可以是常量、变量、表达式等。

```kotlin
val score = 90
val rank = when (score) {
    in 90..100 -> "优秀"
    in 80..89 -> "良好"
    in 70..79 -> "中等"
    in 60..69 -> "及格"
    else -> "不及格"
}
```

这里也看到，`when`是一个表达式，可以直接赋值给一个变量。

同样，`if`也是一个表达式，可以直接赋值给一个变量。

```kotlin
val score = 90
val result = if (score >= 60) "及格" else "不及格"
```

在循环方面，Kotlin也引入了一些新的语法，如`for`循环、`while`循环等。`for`循环可以遍历任何实现了`Iterable`接口的对象，如数组、集合等。

```kotlin
val names = listOf("Alice", "Bob", "Charlie")
for (name in names) {
    println(name)
}
```

这些都不重要，看到能够理解应该就够了。


## 面向对象编程

Kotlin首先是完全继承了Java的面向对象特性的，所以，Kotlin中的类、对象、接口、继承、多态等概念都和Java中的一样。但是，Kotlin中也引入了一些新的特性，如数据类、枚举类、对象类等。

### 数据类

数据类是Kotlin中的一个特殊类，用于存储数据。数据类会自动生成`equals()`、`hashCode()`、`toString()`等方法，使得数据类更加易于使用。

```kotlin
data class User(val name: String, val age: Int)
```

这里定义了一个数据类`User`，包含两个属性`name`和`age`。这样，我们就可以直接创建一个`User`对象，而不需要手动实现`equals()`、`hashCode()`等方法。

```kotlin
val user1 = User("Alice", 18)
val user2 = User("Alice", 18)
println(user1 == user2) // true
```

能用则用，简直太香了。

###  枚举类

枚举类是Kotlin中的一个特殊类，用于表示一组常量。枚举类可以包含多个枚举常量，每个枚举常量都有一个名称和一个值。

```kotlin
enum class Color {
    RED, GREEN, BLUE
}
```

这里定义了一个枚举类`Color`，包含三个枚举常量`RED`、`GREEN`、`BLUE`。这样，我们就可以直接使用枚举常量，而不需要手动定义常量。

```kotlin
val color = Color.RED
```

这个也比Java的香太多， 转换到数字，转换到字符串，都是一行代码的事情。

### 对象类

在Java中，单例有的时候是一个很重要的设计模式。Kotlin直接增加了一个`object`关键字，用于定义单例对象。

```kotlin
object Singleton {
    fun sayHello() {
        println("Hello, Singleton!")
    }
}
```

这里定义了一个单例对象`Singleton`，包含一个`sayHello()`方法。这样，我们就可以直接使用单例对象，而不需要手动实现单例模式。

```kotlin
Singleton.sayHello()
```

这个也是非常香的，不需要写那么多的代码，直接就是一个单例对象。


## 函数式编程

Java是一个面向对象的编程语言，而Kotlin是一个面向对象和函数式编程的编程语言。Kotlin中的函数是一等公民，可以作为参数、返回值、变量等使用。

### 高阶函数

高阶函数是Kotlin中的一个重要概念，指的是可以接受函数作为参数、返回函数的函数。高阶函数可以使代码更加简洁、易读。

```kotlin
fun add(a: Int, b: Int): Int {
    return a + b
}

fun subtract(a: Int, b: Int): Int {
    return a - b
}

fun calculate(a: Int, b: Int, operation: (Int, Int) -> Int): Int {
    return operation(a, b)
}
```

这里定义了两个函数`add()`和`subtract()`，分别用于加法和减法。然后定义了一个高阶函数`calculate()`，用于计算两个数的和或差。

```kotlin
val sum = calculate(1, 2, ::add)
val difference = calculate(1, 2, ::subtract)
```

这里全局函数`add`和`subtract`都是函数类型`(Int, Int) -> Int`，所以可以直接传递给`calculate`函数。并且`::`是函数引用操作符，可以获取函数的引用。实际上，Kotlin利用了Java的静态类的静态方法的特性，将函数作为一个静态方法传递给了`calculate`函数。

### Lambda表达式

对于上面那个例子，我们还可以用一些更加魔幻的调用方式：

```kotlin
val chaos = calculate(1, 2, { a, b -> a*b + a / b })
```

这里，`{ a, b -> a*b + a / b }`就是一个Lambda表达式，用于计算两个数的乘积加上商。Lambda表达式是一种匿名函数，可以作为参数传递给函数。Lambda表达式的语法是`{ 参数列表 -> 函数体 }`，参数列表和函数体之间用`->`分隔。

进一步，Kotlin提供了一个极其变态的语法糖，上面的调用可以写成：

```kotlin
val chaos = calculate(1, 2) { a, b -> a*b + a / b }
```

这看起来就像是一个方法定义，但是实际上是一个方法调用。这个语法糖是Kotlin中的一个特性，使得代码更加简洁、易读（并不是~~~）


### 引用对象的函数

全局的方法，我们用`::`来引用，那么对于对象的方法，我们可以直接引用：

```kotlin
class Calculator {
    fun add(a: Int, b: Int): Int {
        return a + b
    }
}

val calculator = Calculator()
val sum = calculator::add
```

这里，`calculator::add`就是引用`calculator`对象的`add`方法。这样，我们就可以直接使用`sum`变量来调用`add`方法。

```kotlin
val result = sum(1, 2)
```

### 运算符重载

这个是Kotlin的一个特性，可以为现有的类添加新的运算符。运算符重载可以使代码更加简洁、易读。

这里我们就介绍一个重载，就是把一个类伪装成一个函数。

```kotlin
data class Point(val x: Int, val y: Int) {
    operator fun invoke(): String {
        return "($x, $y)"
    }
}
```

这里定义了一个数据类`Point`，包含两个属性`x`和`y`。然后重载了`invoke`运算符，使得`Point`类可以像函数一样调用。

```kotlin
val point = Point(1, 2)
println(point()) // (1, 2)
```

### DSL之假装调用奇怪的东西

当我们把重载调用操作符和Lambda表达式结合起来，就可以创建一种奇怪的东西：

```kotlin
class Calculation {
    var result: Int = 0

    operator fun plusAssign(value: Int) {
        result += value
    }
}

class CalculationScope {
    operator fun invoke(block: Calculation.() -> Unit): Calculation {
        val calculation = Calculation()
        calculation.block()
        return calculation
    }
}
```

这里定义了一个`Calculation`类，包含一个`result`属性和一个`plusAssign`运算符重载。然后定义了一个`CalculationScope`类，包含一个`invoke`运算符重载。

这个方法调用的是`Calculation`的一个扩展函数，这个扩展函数是一个Lambda表达式，这个Lambda表达式的接收者是`Calculation`对象。这样，我们就可以使用`CalculationScope`类创建一个`Calculation`对象，并使用`plusAssign`运算符重载。



```kotlin
val cs = CalculationScope()
val calculation = cs() {
    result += 1
    result += 2
    result.plusAssign(3)
}

println(calculation.result) // 6
```

首先，我们创建了一个`CalculationScope`对象`cs`，然后使用`cs`对象创建了一个`Calculation`对象`calculation`。


在`calculation`的扩展函数（`block`）中，我们使用了`plusAssign`函数，直接操作了`result`。这简直是三观丧尽、斯文扫地……我们程序员都看不懂了。

这个玩意是Kotlin实现各种奇葩DSL的基础：

- 扩展函数
- 运算符重载
- Lambda表达式
- 接收者类型


## Jetpack Compose的DSL

Jetpack Compose是一个基于Kotlin的DSL（领域特定语言），用于构建用户界面。DSL是一种专门用于某个领域的编程语言，用于简化特定领域的编程任务。

Jetpack Compose的DSL是基于函数式编程的，使用函数来构建用户界面。这种DSL的设计使得Jetpack Compose非常灵活、易用。

这里就详细介绍了，后面会在恰当的时候再介绍。

## 总结

其他需要掌握的知识，如协程、Kotlin标准库、Kotlin特性等，都是Kotlin的高级特性，对于Compose Desktop的开发这个学习阶段并不是必须的。

