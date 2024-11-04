+++
title = 'Jetpack Compose for Desktop-003 声明式界面开发'
date = 2024-11-04T20:17:30+08:00
draft = false
mathjax = false
categories = ['jetpack']
tags = ['jetpack', 'compose-desktop', 'kotlin']
toc = true
tocBorder = true
+++



## 概念本源


在界面程序开发中，有两个非常典型的编程范式：命令式编程和声明式编程。命令式编程是指通过编写一系列命令来描述程序的运行逻辑，而声明式编程则是通过编写一系列声明来描述程序的状态。在命令式编程中，程序员需要关心程序的执行过程，而在声明式编程中，程序员只需要关心程序的状态。在界面开发中，声明式编程的优势尤为明显，因为界面开发的本质就是描述界面的状态。

### 命令式编程范式

举个简单的例子，图形界面通常需要考虑的问题是把一堆界面元素组成合理的树状结构，在命令式编程中，我们的做法看起来是下面这样的伪代码：

```kotlin
val root = Container()
val label = Label()
val button = Button()
val panel = Panel()

panel.add(label)
panel.add(button)
root.add(panel)

root.show()
```

这很合理，这个命令式编程的代码描述了我们创建一系列对象，包括容器、面板、界面元素，然后通过结构调整他们的相互关系，构成界面。在完成构造之后，我们调用 `root.show()` 来显示这个界面。这个过程中，我们需要关心的是对象的创建、对象的关系、对象的显示，这是一个过程性的描述。

### 声明式编程范式

其实最常见的声明式编程范式就是 HTML，HTML 是一种标记语言，它的本质是一种声明式的描述，我们通过 HTML 来描述界面的结构，而不是描述界面的构造过程。下面是一个简单的 HTML 代码片段：

```html
<!DOCTYPE html>

<html>
<head>
    <title>My First HTML Page</title>
</head>
<body>
    <h1>Hello, World!</h1>
</body>
</html>
```

好多图形界面开发的程序，Qt或者WPF都采用XML来描述界面，这样的方式也是声明式的。在这种方式中，我们只需要关心界面的结构，而不需要关心界面的构造过程。这种方式的优势在于，我们可以更加专注于界面的结构，而不需要关心界面的构造过程。

## Jetpack Compose的声明式界面开发

在Jetpack Compose中，提出了一个概念就是可组合的声明式界面开发。

比如，描述一列标签构成的界面，我们可以这样写：

```kotlin
@Composable
fun Greeting(name: String) {
    Text(text = "Hello $name!")
}


@Composable
fun MyScreenContent(names: List<String> = listOf("Android", "there")) {
    Column {
        for (name in names) {
            Greeting(name = name)
            Divider(color = Color.Black)
        }
    }
}
```

这里，通过`@Composable`注解，我们定义了一个可组合的函数`Greeting`，这个函数接受一个字符串参数，然后返回一个`Text`组件。然后我们定义了一个`MyScreenContent`函数，这个函数接受一个字符串列表参数，然后返回一个`Column`组件，这个`Column`组件包含了一系列的`Greeting`组件和`Divider`组件。这样，我们就完成了一个简单的界面的描述。

## 深入一下

这样的方式就好像是XML这样的结构化文档，但是有是能够运行的代码。非常有意思，最好玩的是还能通过循环、判断来动态生成界面，这样的方式非常灵活，而且非常容易理解。

那么，这个玩意是如何实现的呢？

在前面[Kotlin旋风之旅](002-KotlinTour.md)中，我们提到了Kotlin的DSL，这个DSL就是Jetpack Compose的核心。

Jetpack Compose的核心思想就是通过实现一种专门用于描述界面的DSL，开发人员通过这套DSL来描述和生成界面。

下面我们也试着用Kotlin的DSL来实现一个简单的DSL，通过这个DSL实现过程，对Jetpack Compose的实现原理有一个祛魅的过程，神秘感不那么强，调试的过程也会更加容易。

### 简化家族树DSL

我们要实现的是一种单体繁殖类人外星生物（也称为`Person`，我们绝对不搞种族歧视）的家族树DSL，这个DSL的结构如下：

```kotlin
fun main() {
    Person("Alice", 80) {
        Children {
            name = "Tom"
            age = 50
            Children {
                name = "Jerry"
                age = 25
                Children("Tom", 2)
            }

            Children {
                name = "Yan"
                age = 15
            }

            // 年龄写错了，改一下
            age = 53
        }

        Children(name = "Tim", age = 40) {
            Children(name = "Jerry", age = 5)
            Children(name = "Alex", age = 15)
        }

        // 调用输出函数，打印家族树
        print()
    }
}
```

这个描述的家族树大概是：

```
    |___Name: Bob, Age: 60
        |___Name: Tom, Age: 50
            |___Name: Jerry, Age: 25
                |___Name: Tom, Age: 2
            |___Name: Yan, Age: 15
        |___Name: Tim, Age: 40
            |___Name: Jerry, Age: 5
            |___Name: Alex, Age: 15
```

可以看到这个代码有几个特点：

1. 通过`Person`函数来描述一个人，这个函数接受一个名字和年龄，然后通过`Children`函数来描述这个人的孩子。
2. 名字和年龄可以省略，也可以通过`name`和`age`参数来指定。
3. 描述的过程中，如果需要修改，也能通过`name`和`age`参数来修改。
4. 能够调用`print`函数来打印自己的家族树。

这个DSL看起来非常简单，其实非常强大。这样就能够把一个家族树描述成跟其天然结构非常接近的、合法的Kotlin代码。

### 实现DSL

这个东西是怎么实现的呢？现给出完整代码：

```kotlin
class PersonImpl(n: String = "", a: Int = 0) {
    var name: String = n
    var age: Int = a

    private fun nBlank(indent: Int) = " ".repeat(indent)

    fun print(indent: Int = 0) {
        print("${nBlank(indent)}|___")
        print("Name: $name, ")
        println("Age: $age")
        for (child in children) {
            child.print(indent + 2)
        }
    }

    private val children = mutableListOf<PersonImpl>()

    fun addChildren(name: String, age: Int, block: PersonImpl.() -> Unit = {}) {
        val child = PersonImpl()
        child.name = name
        child.age = age
        child.block()
        children.add(child)
    }

    operator fun invoke(block: PersonImpl.() -> Unit = {}): PersonImpl {
        block()
        return this
    }


}

fun Person(name: String = "", age: Int = 0, block: PersonImpl.() -> Unit = {}) = PersonImpl(name, age)(block)

fun PersonImpl.Children(name: String = "", age: Int = 0, block: PersonImpl.() -> Unit = {}) =
    addChildren(name, age, block)
```

### 代码解析

上面的调用`Person`的方式要能实现，就需要定义一个函数，它包括三个参数，并且最后一个参数必须是一个能够接受某个类型的函数。

所以`fun Person(name: String = "", age: Int = 0, block: PersonImpl.() -> Unit = {}) = PersonImpl(name, age)(block)`符合这个要求，这也是一个Kotlin的语法糖，单行函数的函数定义。

同时，这个函数申明还省略了返回值类型，这是因为Kotlin的类型推导能力很强，编译器能够根据函数体的返回值类型推导出函数的返回值类型。

写成完整的函数形式，并且把构造对象和调用函数分开来写，是这样的。

```kotlin
fun Person(name: String = "", age: Int = 0, block: PersonImp.() -> Unit={}) {
    val p = PersonImp(name, age)
    p(block)
}
```

这个函数提供了调用`Person(){}`的方式，在大括号里面的代码，针对一个`PersonImpl`实例进行操作，这种方式称为接受者函数字面值。这个功能的实现，我猜要依赖于扩展函数的特性，相当于零时定义一个对象的扩展函数，并且在函数体内部可以直接访问这个对象的属性和方法。

当然，要能够想函数一样调用这个新建的对象，就需要在`PersonImpl`类中定义一个`invoke`操作符函数，这个函数的返回值是`PersonImpl`，这样就能够实现`Person(){}`的调用方式。

接下来就是`Children`函数，这个函数的作用是为一个`PersonImpl`对象添加一个孩子，这个函数的实现也是类似的，通过`addChildren`函数来实现。

```kotlin
fun PersonImpl.Children(name: String = "", age: Int = 0, block: PersonImpl.() -> Unit = {}) =
    addChildren(name, age, block)
```

这个实现了一个扩展函数，这个函数因此只能在`PersonImpl`对象上调用，当然，前面那个接受者函数的代码block里面，所有的调用都是针对`PersonImpl`对象的。

其他普通的构造函数、默认参数、属性、方法等等，都是普通的Kotlin代码，没有什么特别的。


## 总结

在深入进行Jetpack Compose的学习之前，我们先通过一个简单的DSL实现，了解了Jetpack Compose的核心思想：通过声明式的DSL来描述界面。这样的方式非常灵活，而且非常容易理解，也非常容易调试。通过这样的方式，我们可以更加专注于界面的结构，而不需要关心界面的构造过程。

这个实现的过程中，两个语法糖要自己在大脑里反复转换，最后一个参数是匿名函数，则可以移到括号外面；接受者匿名函数相当是临时定义一个扩展函数。

有一点点绕，但是多改改代码，也能够理解。

接下来，就要开始真正的Jetpack Compose的学习之旅了。