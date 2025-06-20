+++
title = 'Go Functional with Rust中的函数式编程'
date = 2025-06-13T15:41:35+08:00
draft = false
mathkatex = true
categories = ['rust']
tags = ['rust', 'functional']
toc = true
tocBorder = true
+++


## 函数式编程的爱与痛

函数式编程（Functional Programming）是一种编程范式，它将计算视为数学函数的求值，避免状态变化和可变数据。函数式编程强调纯函数（Pure Functions），即没有副作用（Side Effects）的函数，只依赖于输入参数，返回值只取决于输入参数，不依赖于外部状态。

2023年的CONISOFT上，有一篇文章称：近年来，函数式编程（FP）重新引起了人们的兴趣，它已经成为包括 Python 和 JavaScript 在内的多种编程语言中流行的编程范式。不仅如此，函数式编程还是 Clojure 和 Haskell 等语言的主要编程范式，这些语言在研究人员和开发者中都变得越来越重要。函数式编程在软件开发中有很多好处，能够帮助我们构建可读性强、易于维护且可扩展的系统。
不过，尽管有这些优势，目前大多数关于软件设计的文献都缺乏对函数式范式的清晰和详细的说明。相比之下，面向对象编程（OOP）就拥有大量的设计资源和工具。这些挑战，再加上其他因素，使得想要在系统中采用函数式编程的开发者们感到困难。

函数式并不是一个新概念，早在1930年代，数学家阿隆佐·邱奇（Alonzo Church）就提出了lambda演算，这是函数式编程的数学基础。Haskell语言的创始人菲利普·瓦德勒（Philip Wadler）1988年参与撰写了《Introduction to Functional Programming》。

![Philip Wadler](<https://homepages.inf.ed.ac.uk/wadler/Pics/phil-portrait-2021.jpg>)

跟函数式编程相关的几个重要概念包括：不可变、高阶函数、闭包、惰性求值、递归、组合式、模式匹配。这些概念在现代、新的编程语言中广泛存在，例如Rust中，函数式编程的特性随处可见。

## Rust的函数式编程工具

### 函数是第一等公民

在Rust中，函数是第一等公民，可以作为参数、返回值、赋值给变量，并且可以作为结构体、枚举的成员。函数的定义方式也包括了常规的函数定义`fn`，以及闭包lambda定义`||`的形式。

```rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

Rust的函数实际上比C语言的函数简单多了，没有重载，没有任何奇怪的东西；Rust的结构体，甚至连C++中的构造函数、析构函数都不存在。

所有的时候，所有的函数，全部都是一样的。

### lambda

```rust
let add = |a: i32, b: i32| a + b;

let apply = |f, x| f(x);

fn add_func(step: i32) -> impl Fn(i32) -> i32 {
   fn add(x: i32) -> i32 {
    x + step
   }
   add
}

#[test]
fn test_add_func() {
    let add_1 = add_func(1);
    let add_2 = add_func(2);

    assert_eq!(add_1(10), 11);
    assert_eq!(add_2(10), 12);
}
```

### 表达式

表达式在函数式编程中也是有一个非常核心的地位，更方便地构造和使用表达式，才能更好的实现函数式编程。

Rust，很多人认为有点太过了，表达式化得非常彻底。除`let`之外，所有的语法构造都是表达式。

分支语句是表达式，所以可以作为返回值。

```rust
let a = if 1 > 2 {
    "a"
} else {
    "b"
}

let x = 2;
let y = match x {
    1 => 1,
    2 => 2,
    _ => 3,
}
```

循环语句是表达式，所以可以作为返回值。

```rust
// 给出循环控制结构作为语句的例子
let a = loop {
    break 1;
}
let b = while 12 > 2 {
    break 2;
}
let c = for i in 0..10 {
    i
}

```

关于循环语句作为表达式，到底有啥好处？我不是很理解……

当然，语句块也是表达式，所以可以作为返回值，最后一个语句的值就是语句块的值。

```rust
let a = {
    let x = 1;
    let y = 2;
    x + y
}
```

所以还有这样的`main`函数：

```rust
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Hello, world!");
    // do stuff that might fail
    // for example, read a file
    let file = File::open("Cargo.toml")?;
    let mut reader = BufReader::new(file);
    let mut line = String::new();
    reader.read_line(&mut line)?;
    println!("line: {}", line);
    // return a default OK at last
    Ok(())
}
```

这个时候，如果文件不存在，就会返回一个错误。在终端中显示：

```shell
Hello, world!
Error: Os { code: 2, kind: NotFound, message: "系统找不到指定的文件。" }
```

也是一种很简单的错误处理方式。

当然，各种各样的表达式之后，`lambda`写起来就非常方便了。

### 高阶函数和迭代器

Rust中个还有一个非常函数化的就是有非常好用的迭代器，可以配合高阶函数与lambda函数，实现非常紧凑的编码形式，也就是常说的声明式编程。

```rust
    let a = (0..10)
        .map(|x| x * 3)
        .inspect(|x| println!("x: {}", x))
        .collect::<Vec<i32>>();
    // [0, 3, 6, 9, 12, 15, 18, 21, 24, 27]

    let b = a
        .iter()
        .map(|x| x * 2)
        .filter(|x| x % 2 == 0)
        .inspect(|x| println!("x: {}", x))
        .collect::<Vec<i32>>();
    // [0, 6, 12, 18, 24]

    let c = a
        .iter()
        .map(|x| x * 2)
        .filter(|x| x % 3 == 0)
        .inspect(|x| println!("x: {}", x))
        .collect::<Vec<i32>>();
    // [0, 6, 12, 18]
```

当然，内部的实现还是循环，只不过是迭代器内部实现的循环。

## 总结

Rust在C的基础上，提供的超模工具有三个：

- `trait`和泛型
- 宏（两种宏）
- 函数式编程

这其中，函数式编程的部分虽然没有正牌的函数式编程语言那么强大，但是配合`trait`和泛型，可以让人在底层编程中更加舒服亿点点。
