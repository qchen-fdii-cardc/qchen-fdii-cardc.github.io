+++
title = 'Math and Mermaid in Rustdoc中使用公式和Mermaid绘图'
date = 2025-04-25T14:27:56+08:00
draft = false
mathkatex = true
categories = ['rust']
tags = ['rust', 'cargo', 'rustdoc', 'math', 'mermaid', 'katex', 'markdown']
toc = true
tocBorder = true
+++

## Rust神启动

又是Rust神启动的一天，今天我们要在Rustdoc中使用公式和Mermaid绘图。

本来我是准备蹭[Cangjie](<https://cangjie-lang.cn/>)的热度，搞一个领域程序的自主可控、国产化替代、数智赋能、降本增效的项目。就因为在[知乎](<https://www.zhihu.com/question/8452441330/answer/1898461142709417202>)搞抽象，整了一个[Rust更新那啥hosts文件](/posts/rust/update-hosts)，结果被带偏了，现在咋办呢？我要搞一个内存安全、开源可控、国产化替代、降本增效、数智赋能、自主可控的领域数字化项目。

反正总是要蹭，蹭上水就是谁！

## 学Rust从哪里开始？

我以前听侯捷老师讲过一点点Rust，看过好几本Rust书的封面，还从没看过内容。从哪里开始学呢？

必然就是：[doc.rust-lang.org](<https://doc.rust-lang.org/>)

点开一看，不说别的，审美和一致性就让人不由得眼前一亮。虽然锈粉也干过很多抽象的事情，但是Rust这个审美真是强迫症福音，没毛病。

特别是那个[Rust API documentation](<https://doc.rust-lang.org/stable/std/index.html>)，是我这几天学习的很重要的对象，导航比较清晰，还能直接看源代码。

就比如那个`pub trait Copy`，点进去一看。

![Rust API documentation](/rust/rustdoc-math-mermaid/doc-copy.png)

这个帮助也是非常完善，上面有`Source`超链接，点进去一看，源代码是这样的：

```rust
pub trait Copy: Clone {
    // Empty.
}
```

这就是个名字，还得回去看`Clone`的定义。恰好，前面的代码中间有`Clone`的定义，也可以点[Clone](<https://doc.rust-lang.org/stable/std/clone/trait.Clone.html>)。

进去之后一样有源代码。

这么好的文档，不学Rust都对不起自己。原神启动！

## API文档是怎么形成的？

Q: 这么好看的Rust API文档是怎么形成的？

A：Rust的API文档是根据源代码（注释）自动生成的。

好吧，这个答案并不让人意外。在Rust的源代码中，可以通过编写特殊的注释，来生成API文档。

```rust
/// 这是一个示例
///
/// ```rust
/// let x = 1;
/// ```
```

而生成API文档的工具，就是`rustdoc`。有一本书：[The rustdoc book](<https://doc.rust-lang.org/rustdoc/what-is-rustdoc.html>)，就是专门介绍`rustdoc`的。好好看去吧。

我是很快就发现，在`lib.rs`中，可以增加`#[doc = include_str!("doc/doc.md")]`，来包含一个markdown文件。

并且，每个`mod`，也可以增加`#[doc = include_str!("doc/mod.md")]`，来包含一个markdown文件。

这我就不困了……很快，我的蹭热度项目的代码的文档就开始起飞了……

每次编了一些文档，就可以运行：

```bash
cargo rustdoc --open     # 可以这样
cargo doc --open         # 或者这样
```

自动打开浏览器，就可以看到文档了。我喜欢把设计思路都写在文档中，然后一边看设计文档，一边改文档和代码。

紧接着，问题来了，我发现，这个被包含的md文件中的公式和Mermaid绘图，怎么就不好使了呢？

## 公式和Mermaid绘图

就比如说，我想要这样。行内公式、行间公式、Mermaid绘图。

![Rust API documentation](/rust/rustdoc-math-mermaid/rustdoc-index.png)

### 解决方案

已知：可以在`rustdoc`中采用`--html-in-header`选项，来在`head`中增加内容到生成的网页中。

```bash
cargo rustdoc --open -- --html-in-header my-header.html
```

这里就只能用`rustdoc`了，`cargo doc`不行。

那么对我这样的`Javascript`程序员来说，一切皆有可能了。。。

- [rustdoc-header.html](/rust/rustdoc-math-mermaid/rustdoc-header.html)

```html
{{% codeseg "/static/rust/rustdoc-math-mermaid/rustdoc-header.html" %}}
```

这个HTML就很容易的把`katex`和`mermaid`的CSS和JS加载进来。

当然，现在我们的示例代码，就可以这样写：

```rust
{{% codeseg "/static/rust/rustdoc-math-mermaid/rustdoc-tutorial/src/main.rs" %}}
```

而对应的`README.md`，就可以这样写：

    {{% codeseg "/static/rust/rustdoc-math-mermaid/rustdoc-tutorial/README.md" %}}

完美！

### 完美之后的一点点不完美

现在，我们每次都要在运行`cargo rustdoc`后面增加参数`-- --html-in-header rustdoc-header.html`，太麻烦了。

所幸，Cargo还提供了配置的方式，在`Cargo.toml`同一个目录下面，或者在需要特殊出路的`mod`目录下面，增加一个配置文件夹`.cargo`，在这个文件夹中增加一个`config.toml`文件，就可以配置`rustdoc`了。

```toml
{{% codeseg "/static/rust/rustdoc-math-mermaid/rustdoc-tutorial/.cargo/config.toml" %}}
```

这样，我们每次运行`cargo rustdoc`，就会自动增加`--html-in-header rustdoc-header.html`参数。

完美！

## 总结

Rust的API文档是根据源代码自动生成的。

在Rustdoc中使用公式和Mermaid绘图，可以通过配置`rustdoc-header.html`来实现。

在Cargo中配置`config.toml`，可以自动增加`--html-in-header rustdoc-header.html`参数。
