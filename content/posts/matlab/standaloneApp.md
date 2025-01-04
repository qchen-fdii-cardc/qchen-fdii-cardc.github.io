+++
title = 'StandaloneApp in Matlab中发布独立的App'
date = 2024-12-31T10:45:49+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'app', 'standalone', 'deploy']
toc = true
tocBorder = true
+++

## Matalb中应用部署

Matlab因为年头久远，所有的东西都积累了一大堆。就说是应用部署，Matlab 2023b至少有下面的几个技术线

- Matlab Compiler技术线：产生独立App可执行程序或者网页应用
- Simulink Compiler技术线：产生独立App可执行程序，或者FMU（Functional Mock-up Unit）
- MATLAB Compiler SDK技术线：产生软件组件
- MATLAB production Server技术线：产生Web服务、数据库和企业应用
- MATLAB Web App Server技术线：产生Web应用，支持MATLAB和Simulink

这是大的技术线划分，实际上，MATLAB的应用可以部署为：

- 独立运行的（桌面）应用
- Web应用
- Microsoft Excel插件
- 大数据分析应用
- 微服务
- 企业应用和云服务
- 打包成库，供其他语言调用（Java、.Net， Python，C/C++）

所以，别问什么事情在MATLAB中行不行，就问你想要什么。只要给够钱，牛马们都能干。

打钱热线：666-666-6666

## 简单的独立App例子

下面给出一个简单的命令行应用打包的例子，为了展示过程，我们先整一个最简单的TUI应用，然后打包成独立App。

这个应用的功能超级牛叉，应用可以接受0个或者1个字符串参数，输出你好世界，来自xxx。当提供参数时，这个xxx就是参数，否则就是默认值：MATLAB。



```matlab
{{% codesnap "static/matlab-code/standaloneAppTutor/hello.m" %}}
```

这个程序可不要小看，是业界鼎鼎大名的Hello World程序，所有致力于修仙的小卡拉米首先都要把这个玩意整出来。跟别说，我们MATLAB仙人还提供了默认参数的选项，顿时把这个冒烟测试的例子提升到它不应该有的高度。

### 编译

首先，我们要把这个程序编译成可执行程序。请各位仙路上的同学们打开MATLAB，然后输入下面的命令：

```matlab
help compiler.build.standaloneApplication
```

这个命令接受一个MATLAB函数文件的文件名作为参数，然后编译出可执行程序。当我们的函数文件是`hello.m`时，我们可以这样调用：

```matlab
compiler.build.standaloneApplication('hello.m')
```

这个程序会在当前目录下申城一个`hellostandaloneApplication`的文件夹，里面包含了编译好的可执行程序：`hello.exe`（Windows）。

此外，这个目录里大概包含以下文件：

- `hello.exe`：可执行程序[hello.exe](/matlab-code/standaloneAppTutor/hellostandaloneApplication/hello.exe)
- `includeSupportPackages`：支持包列表，这里什么包都没有，字节0
- `readme.txt`：说明文件
- `mccExcludedFiles.log`：编译时排除的文件列表
- `requiredMCRProducts.txt`：所需要的MATLAB Runtime产品
- `unresolvedSymbols.log`：编译时未解决的符号列表


```
{{% codesnap "static/matlab-code/standaloneAppTutor/hellostandaloneApplication/readme.txt" %}}
```

这个`readme.txt`文件里面包含了一些关于这个应用的信息，比如如何安装MATLAB Runtime，如何运行这个应用等等。

这里最重要的就是安装这个MCR，我们可以从`requiredMCRProducts.txt`中产看版本信息，然后去MATLAB官网下载对应的MCR安装包。

```
{{% codesnap "static/matlab-code/standaloneAppTutor/hellostandaloneApplication/requiredMCRProducts.txt" %}}
```

其他文件信息不再赘述。

### 安装包

当然，我们可以根据上面的`readme.txt`来安装这个应用，但是这个过程太繁琐了，我们可以把这个应用打包成一个安装包，然后一键安装。

这里我们就需要另外一个工具。修仙的朋友们请输入：

```matlab
help compiler.package.installer
```

我们可以调用这个函数来产生一个安装包，这个包里最重要的参数有两个：

- 第一个不具名参数：要打包的应用，直接用前面编译好的结果就行
- `RuntimeDelivery`：MATLAB Runtime的安装包的提供方式，可以是`web`，可以是`installer`。

当选择`web`时，这个安装包会包含一个URL，用户安装时会提示从这个URL下载MCR安装包。当选择`installer`时，这个安装包会包含MCR安装包，用户可以直接安装。

```matlab
{{% codesnap "static/matlab-code/standaloneAppTutor/buildHello.m" %}}
```

这里我们展示了两种调用方式，`web`的方式会产生一个很小的安装文件：

- [helloInstaller.exe](/matlab-code/standaloneAppTutor/helloinstaller/HelloInstall.exe)

另外一种方式会产生一个接近500M的玩意，包含了所需要的MCR安装内容，直接安装即可使用。

这两个文件的大小参见：

![helloInstaller](/matlab-img/standaloneApp/installer.png)

## 总结

1. 首先，一定要相信MATLAB，它能做到一切，但是得加钱！
2. 其次，MATLAB的应用部署技术线很多，不同的应用场景有不同的选择。
3. 这个TUI的可执行文件还很小的，只有1.2M，用起来也很方便。
