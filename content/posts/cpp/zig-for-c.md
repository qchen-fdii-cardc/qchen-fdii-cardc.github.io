+++
title = 'C语言三分钟：用Zig工具链管理C程序main函数'
date = 2025-01-16T15:27:28+08:00
draft = false
mathjax = false
categories = ['cpp']
tags = ['cpp', 'zig', 'c', 'addExecutable', 'build.zig']
toc = true
tocBorder = true
+++


## 依然是你好世界

我们还来从`你好世界！`开始。我们用makefile或者CMake来管理C语言程序是挺好的，但是我们就是要干一点不一样的事情。

我们要用C语言来写一个程序（编辑），但是用zig来完成编译链接、调试运行和发布。

```C
{{% codesnap "static/zig/hellocpp/src/main.c" %}}
```

其实Zig天然就能当C/Cpp语言编译器工具链，其实底下是clang。

```shell
zig cc src/main.c -o hello
```

就这样可以直接得到可执行的`hello`，或者是`hello.exe`（Windows平台）。并且这个可执行文件还比直接用`gcc`编译的可执行文件小很多……真的小很多。

这不是什么重要的优点，优点反而是Zig和C的混合编程，Zig的工具链可以很好的管理C程序，而且Zig的工具链还是很现代化的。


## Zig和C

我们首先用Zig的`init`命令来初始化一个Zig项目。

```shell
zig init hellocpp
```

然后可以把`src`目录下的两个文件删除，把`main.c`文件放到`hellocpp/src`目录下，然后我们修改`build.zig`文件程如下的样子。

```Zig
{{% codesnap "static/zig/hellocpp/build.zig" %}}
```

现在就能够用`zig build`来编译我们的C程序了。

```shell
zig build
```

在`zig-out/bin`目录下就能看到`hellocpp`可执行文件了。

```shell
./zig-out/bin/hellocpp
```

当然，我们还可以直接用`zig build run`来运行我们的程序。

```shell
zig build run
```

这样就能够用Zig的工具链来管理我们的C程序了。


## 总结

找了Zig的英文和中文文档，都没有找到这个功能的介绍。只有一个帖子是抱怨没办法用Zig来管理C程序的`main`函数。在`0.13.0`版本之后，Zig支持得挺不错的。

后面我们更改程序，可以直接完成调试运行和发布。

那么，我这么做的初衷是什么呢？根本上，我就是想利用Zig来写C语言函数的单元测试，毕竟Zig的测试真的香。