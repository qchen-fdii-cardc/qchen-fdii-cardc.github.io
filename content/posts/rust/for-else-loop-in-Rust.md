+++
title = 'For Else Loop in Rust如何实现for-else语法'
date = 2025-05-02T15:51:22+08:00
draft = true
mathkatex = true
categories = ['rust']
tags = ['rust', 'for-else', 'loop', 'macro']
toc = true
tocBorder = true
+++

## 形而下之，下不为例

>> Rust中有没有for-else语法？

答案是，没有。

但是，我们可以通过一些方法来实现for-else语法。

## 各种实现方式

### 使用标志位

```rust
let mut found = false;
for i in 0..10 {
    if i == 15 {
        found = true;
        break;
    }
}
if !found {
    println!("Not found");
}
```

这个思路非常简单，循环，如果没有找到，就执行else块。

### `if`特殊用法

```rust
if 'search: loop {
    for i in 0..10 {
        if i == 15 {
            break 'search false;
        }
    }
    true
} {
    println!("Not found");
}
```

这个是在耍赖，if后面是一个语句

```rust
'search: loop {
    for i in 0..10 {
        if i == 15 {
            break 'search false;
        }
    }
    true
} 
```

这个loop表达式返回一个布尔值，如果找到，返回false，否则返回true。

### 函数式

```rust
0..10.iter()
    .find(|&i| i == 15)
    .or_else(|| println!("Not found"));
```

这个是函数式的实现，使用`find`方法，如果找到，返回Some(i)，否则返回None。这个归根结底使用了`Option`类型。

### 略微变态的`if let`

```rust
if let None = (0..10).find(|&i| i == 15) {
    println!("Not found");
}
```

这个是略微变态的实现，使用`if let`语句，如果找到，返回Some(i)。后面还可以加个else块，如果找到了，就执行else块。

### 使用`label`语句块

```rust
'label: {
    for i in 0..10 {
        if i == 15 {
            break 'label false;
        }
    }
    println!("Not found");
}
```

这个是使用`label`语句块的实现，如果找到，就执行`break`语句，否则执行`println!("Not found")`语句。

## 说在最后

其实，Rust中没有for-else语法。上面这些都是一些奇技淫巧，用来实现for-else语法。没有任何意义，最好就是写点最平实无华的代码。

好吧，我们其实还能写一个宏来实现for-else语法。

```rust
{{% codeseg "static/rust/for-else-loop-in-rust/src/lib.rs" %}}
```

测试的代码：

```rust
{{% codeseg "static/rust/for-else-loop-in-rust/tests/test.rs" %}}
```

不知道这样行不行，当然，还是难看……

## 结论

这还啥结论，Python程序员就不能问这样的问题，有且只有一种正确的方式编程是Python程序员的最大、唯一、不多的美德……
