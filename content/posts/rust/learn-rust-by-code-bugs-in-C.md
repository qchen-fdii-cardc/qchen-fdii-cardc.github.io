+++
title = 'Learn Rust by Code Bugs in C语言实现Rust无法做到的bug'
date = 2025-04-30T14:40:03+08:00
draft = false
mathkatex = true
categories = ['rust', 'c']
tags = ['rust', 'c', 'memory safety', 'concurrency', 'safety', 'bugs','悬垂指针','数据竞争','缓冲区溢出','迭代器失效']
toc = true
tocBorder = true
+++

## Rust的Niche在哪里？

Rust吹得那么凶，大家那么喜欢她，人人都爱她，她的生态位到底在哪里呢？不可能说一个丑女大家都能喜欢。所以，人家还是有点东西的。

![](/rust/learn-rust-by-code-bugs-in-C/niche.png)

《Rust in Action》里面就分析了Rust可以解决四类编程问题：

- 悬垂指针：引用了在程序运行过程中已经变为无效的数据
- 数据竞争：由于外部因素的变化，无法确定程序在每次运行时的行为
- 缓冲区溢出：例如一个只有6个元素的数组，试图访问其中的第12个元素
- 迭代器失效：在迭代的过程中，迭代器中值被更改而导致的问题

其实Rust确实不适合当作第一门语言来学习，因为人只有写过bug才知道Rust这种Bug都不让你写有一点点价值……

## Bug大师：C语言

大家都说C语言是bug大师，这个说法还是很客气的。C语言简直是bug圣斗士、bug之神。其实主要的原因就是C语言自由度非常高，电脑（计算机）能干什么，C语言就让你干，你可以随便干。语言层面上，C非常接近汇编语言乃至机器语言。在C语言中，所有的代码和内存都是平等的……

![](/rust/learn-rust-by-code-bugs-in-C/equal.jfif)

下面我们演示一下Rust写不来的bug。

### 悬垂指针

```c
{{% codeseg "static/rust/learn-rust-by-code-bugs-in-C/hanging_pointer.c" %}}
```

要做一个悬垂指针很简单，申请一块内容，然后把它给释放了，随后就当无事发生，继续用指针访问它，其实计算机根本部不管你这些那些的，你随意编造一个指针地址去访问也是可以的……参考程序13~25行，程序可能打印出来的值还是对的呢！因为计算机也没有必要去把方式内存中的值给清空。其实，C语言还允许你随便乱编造一个不同数据类型的指针去读、写内存呢。这个玩意呢，有一个最佳实践，就是，`free`之后，把指针设置为`NULL`，这样你下次访问的时候，程序就会崩溃（！），你就知道你犯错了。

还有另外两种Rust不行的，就是把局部定义域种的变量、别的函数种的局部变量的地址（指向该变量）返回给，然后使用这个地址。

其实那个栈上的地址也总是存在的，只不过是里面的信息可能（也可能暂时还没有）被覆盖……也就是说，按照道理，对于代码来说，那个地址的信息已经从语义上不存在了……Rust就做不到这个……

### 数据竞争

```c
{{% codeseg "static/rust/learn-rust-by-code-bugs-in-C/racing_conditions.c" %}}
```

这个程序也很简单，不过编译要用c11的标准，才有`threads.h`。

```bash
gcc -std=c11 racing_conditions.c -o racing_conditions
```

如果是msvc，则需要用`/std:c11`。

```bash
cl /std:c11 racing_conditions.c
```

这里，一个局部变量`counter`被三个线程同时访问。两个摸一个看。一个使劲增加它，一个使劲减少它，一个使劲打印它。

要知道，Rust可不允许搞什么群体性活动……如果有一个人摸，别人看都不能看；可以同时有几个人看。

### 缓冲区溢出

```c
{{% codeseg "static/rust/learn-rust-by-code-bugs-in-C/overflow.c" %}}
```

C语言根本不检查数组越界，所以，你随便越界，计算机也管不着。

Rust就不行，不能在自己加外面看、更不能摸！

### 迭代器失效

```c
{{% codeseg "static/rust/learn-rust-by-code-bugs-in-C/loop.c" %}}
```

因为C语言就没有什么迭代器，差不多就这个意思整一下。

在Rust中，不行，不行，一边迭代一个集合，一边修改这个集合是不行的……

## Rust的开发工具

这里呢，就可以看到，学习Rust应该怎么学？找准Rust的Niche，然后，用C语言的思维能做、Rust不能做的的角度来学习Rust。

另外呢，还要介绍一下Rust的一个工具，叫做`cargo`，他有一个功能，叫做`cargo fix`，可以自动试图修复你的错误……真是离谱。

我们整一个悬垂指针：

```rust
{{% codeseg "static/rust/learn-rust-by-code-bugs-in-C/src/main.rs" %}}
```

然后，我们运行一下：

```bash
cargo check
```

这其实已经说的很清楚了，甚至给出了错误的原因。

```bash
error[E0382]: borrow of moved value: `grains`
  --> src\main.rs:23:22
   |
14 |     let mut grains: Vec<Cereal> = vec![];
   |         ---------- move occurs because `grains` has type `Vec<Cereal>`, which does not implement the `Copy` trait
...
21 |     drop(grains);
   |          ------ value moved here
22 |
23 |     println!("{:?}", grains);
   |                      ^^^^^^ value borrowed here after move
   |
   = note: this error originates in the macro `$crate::format_args_nl` which comes from the expansion of the macro `println` (in Nightly builds, run with -Z macro-backtrace for more info)

For more information about this error, try `rustc --explain E0382`.
error: could not compile `learn-rust-by-code-bugs-in-C` (bin "learn-rust-by-code-bugs-in-C") due to 1 previous error
```

然后，我们就可以用`cargo fix`来修复这个错误：

```bash
cargo fix
```

它首先会提示你，别整了，先commit一下再搞。合理！

```bash
error: the working directory of this package has uncommitted changes, and `cargo fix` can potentially perform destructive changes; if you'd like to suppress this error pass `--allow-dirty`, or commit the changes to these files:
```

然后，我们就可以用`--allow-dirty`来修复这个错误：

```bash
cargo fix --allow-dirty
```

不过最好是先commit一下，然后，再`cargo fix`。

结果，它打印出来的信息跟`cargo check`是一样的。

![](/rust/learn-rust-by-code-bugs-in-C/haha.jpg)

我就当无事发生，今天的天气真好。

## 总结

C坏，Rust好……那是不可能的。Rust编程序真的费劲，需要跟编译器使劲解释，搞清楚谁能摸、谁能看，摸的摸多久，看的看到什么时候……

![](/rust/learn-rust-by-code-bugs-in-C/legs.jfif)

Rust主要是后发优势，工具链完善，报错信息（这是因为我们需要给编译器提供大量的信息）完善。
