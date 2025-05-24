+++
title = '正确使用Matlab的Library Compiler'
date = 2025-05-24T10:10:11+08:00
draft = false
mathkatex = true
categories = ['matlab', 'cpp']
tags = ['matlab', 'cpp', 'library_compiler', 'mcr']
toc = true
tocBorder = true
+++


## 概述

MATLAB Library Compiler 是 MATLAB 的一个强大工具，它可以将 MATLAB 函数编译成动态链接库（DLL），使得其他编程语言（如 C/C++）可以调用 MATLAB 函数。这为跨语言集成提供了便利，特别适用于需要在高性能应用中集成 MATLAB 算法的场景。

## 前提条件

在目标计算机上运行编译后的 DLL 之前，必须安装相应版本的 MCR。MCR 是一个独立的运行时环境，不需要完整的 MATLAB 安装。实际上，采用Matla编译到C或者C++的动态链接库文件时，提供会提供MCR的安装包，只需要安装即可。由两种形式，网络安装或者离线安装。

前面一段时间在知乎开玩笑：用Matlab写东西，一句代码也可以跟你搞上一张DVD，简直是诚意满满。

- **编译器**: Visual Studio (Windows) 或 GCC (Linux)
- **CMake**: 用于构建系统管理
- **MATLAB**: 用于编译 MATLAB 函数为 DLL

## 步骤

### 第1步：准备 MATLAB 函数

首先创建要编译的 MATLAB 函数。以下是一个简单的示例：

```matlab
{{% codeseg "static/matlab/easy_dll/easy.m" 1 5 %}}
```

### 第2步：使用 Library Compiler 编译

在 MATLAB 命令窗口中执行以下命令：

```matlab
libraryCompiler
```

至于这玩意怎么用，请不要来问我，我也不知道。编译成功后会生成以下文件：

- `easy.dll` - 动态链接库
- `easy.lib` - 链接库
- `easy.h` - C语言头文件

编译器会自动生成 C 语言接口的头文件，按照这个接口文件，就可以使用函数`easy`，只不过名字变成了`mlfEasy`。这里的`mlf`大概是`Matlab Live Forver`的缩写，当然也可能是`Matlab Love Fucking`的缩写。

```c
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/easy.h" %}}
```

## C 语言集成

以下是调用 MATLAB DLL 的完整 C 程序示例：

```c
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/main.c" %}}
```

关键的步骤和代码：

- 使用 `mxCreateDoubleScalar` 创建输入参数
- 调用 `mlfEasy` 执行 MATLAB 函数
- 使用 `mxGetPr` 获取结果数据

## 构建配置

### CMakeLists.txt 配置

![没苦硬吃警告](/matlab/easy_dll/mkyc1.png)

```cmake
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/CMakeLists.txt"%}}
```

唯一需要注意的就是替换Matlab的安装路径。然后就是一气呵成的

```bash
cmake -B build
cmake --build build --config Release
build\Release\matlab_dll_demo.exe
```

完美。

### 员工通道

实际上，我是故意要把什么CMake写在前面凑字数。Matlab中提供非常简单的工具产生exe文件。

```matlab
mbuild main.c easy.lib
```

这是windows下，linux下是

```matlab
mbuild main.c -L. -leasy
```

当然这两个前提都是把easy.lib/easy.dll和easy.h放在当前目录下。这里面Linux那个`-L.`就是告诉编译器，动态链接库在当前目录下。

我就不这么办，我既要让AI帮我写一个CMakeLists.txt。

## 一个小坑

当然，在使用Matlab编译的DLL时，会有两个小坑，说是1个自然就是2个，四大猛男当然是5个人。

- 一个问题就是必须配对使用两个函数：
  - `mclInitializeApplication`
  - `mclTerminateApplication`
- 另外一个问题就是如果在Matlab代码中使用了多线程或者类似玩意，可能会有坑。所以在上面那个函数调用是可以加上`-singleCompThread`参数。

这个问题在[undocumentedmatlab.com](<https://undocumentedmatlab.com/articles/quirks-with-compiled-matlab-dlls>)有详细说明。

实际上，我作为一个狠人，我还准备了Windows下面的狠活。

```c
// 使用强制终止
#ifdef _WIN32
HANDLE hProcess = GetCurrentProcess();
TerminateProcess(hProcess, 0);
#endif
```

## 总结

MATLAB Library Compiler 为 MATLAB 算法的跨语言集成提供了强大的解决方案。

不要干什么把M文件翻译成dll，然后又在Matlab中用loadlibrary加载的事情。

那个功能是pcode，不是这个，下回再说吧。看过我以前帖子的朋友自然会`help pcode`, `doc pcode`一套连招。
