+++
title = 'Rust for Matlab里面调用Rust函数之为什么要这么做'
date = 2025-05-06T17:24:25+08:00
draft = true
mathkatex = true
categories = ['rust', 'matlab']
tags = ['rust', 'matlab', 'c', 'abi', 'pointer']
toc = true
tocBorder = true
+++

## 因为能

前面写[WASM](/posts/rust/maze-wasm-in-rust/)的时候，发现名为WASM实则是一个C语言的动态链接库。

既然如此，为什么不能用Rust来写一个动态链接库，然后Matlab来调用呢？

虽然，不知道为什么要这么干。实际上，我连用C/C++写一个动态链接库然后Matlab来调用都嫌没必要，最多是写一个可执行文件，然后处理输入到输出，然后matlab调用这个可执行文件。

就当做是没苦硬吃。

## 那就开始

首先，建立一个Rust的库。

```bash
cargo new --lib rs2m
```

然后在`Cargo.toml`中添加

```toml
[lib]
crate-type = ["cdylib"]
```

变成：

```toml
{{% codesnap "static/rust/rust4matlab/Cargo.toml" %}}
```

然后，写一个简单的函数，然后导出。

```rust
{{% codeseg "static/rust/rust4matlab/src/lib.rs" 1 30 %}}
```

注意到这里，我们每定义一个函数，都要在前面加上`#[no_mangle]`，这个是告诉编译器，这个函数不要进行名字修饰，否则在Matlab中调用的时候会找不到。

然后，在函数的返回值前面加上`pub extern "C" fn`，这个是告诉编译器，这个函数是导出的，然后`"C"`表示这个函数是按照C语言的规则来导出的。

普通的值的传递乏善可陈，就是简单的对应关系，可以在[Comparing C/C++ types to Rust](<https://locka99.gitbooks.io/a-guide-to-porting-c-to-rust/content/features_of_rust/types.html>)中找到；然后C/C++和Matlab的对应可以不太在意，因为Matlab是一个工程师的懒人语言，通常不纠结数据类型，除非算不出来。

唯一好玩一点点的就是，浮点数组的传递，这里需要用`*mut f64`来表示，然后Matlab中需要用`libpointer('doublePtr', result)`来表示。这个`*mut f64`是Rust的原始指针类型。

```rust
{{% codeseg "static/rust/rust4matlab/src/lib.rs" 11 30 %}}
```

这个代码中就有对原始指针的两个操作，`is_null()`和`add()`。

`is_null()`是判断指针是否为空，`add()`是获取指针指向的值。我们使用了`unsafe`来操作原始指针指向的值。

```bash
cargo build --release
```

Rust这一边的事情就全部完成。最终我们我们也弄了点测试，假装测试一下。

```rust
{{% codeseg "static/rust/rust4matlab/src/lib.rs" 31 %}}
```

当然，这里测试中的原始指针就通过`Vec<f64>::as_mut_ptr()`来获取。

```bash
cargo test
```

## Matlab与C语言的接口

基本上C语言的接口，ABI，是计算机中非常非常通用的。Matlab也不例外，提供了一系列函数来调用
