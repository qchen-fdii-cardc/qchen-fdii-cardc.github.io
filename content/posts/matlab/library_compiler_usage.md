+++
title = '正确使用Matlab的Library Compiler'
date = 2025-05-24T10:10:11+08:00
draft = true
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
% 方法1：使用 mcc 命令行
mcc -W lib:easy,version=1.0 -T link:lib -d ./output easy.m

% 方法2：使用图形界面
libraryCompiler
```

编译成功后会生成以下文件：

- `easy.dll` - 动态链接库
- `easy.lib` - 链接库
- `easy.h` - C语言头文件

编译器会自动生成 C 语言接口的头文件：

```c
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/easy.h" %}}
```

## C 语言集成

以下是调用 MATLAB DLL 的完整 C 程序示例：

```c
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/main.c" %}}
```

### 关键步骤解析

```c
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/main.c" 25 40 %}}
```

- `mclInitializeApplication`: 初始化 MATLAB 应用程序
- `easyInitializeWithHandlers`: 初始化特定库并设置错误和打印处理器

```c
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/main.c" 45 65 %}}
```

- 使用 `mxCreateDoubleScalar` 创建输入参数
- 调用 `mlfEasy` 执行 MATLAB 函数
- 使用 `mxGetPr` 获取结果数据

```c
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/main.c" 70 105 %}}
```

## 构建配置

### CMakeLists.txt 配置

```cmake
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/CMakeLists.txt" 1 25 %}}
```

```cmake
{{% codeseg "static/matlab/easy_dll/easy/for_redistribution_files_only/CMakeLists.txt" 30 49 %}}
```

### 偷懒办法

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

当然，在使用

**问题**: `mclInitializeApplication` 返回 false
**解决方案**:

- 使用 `-singleCompThread` 参数
- 检查 MCR 版本兼容性
- 确保有足够的系统资源

[undocumentedmatlab.com](<https://undocumentedmatlab.com/articles/quirks-with-compiled-matlab-dlls>)

在某些情况下，程序可能在 `easyTerminate()` 后挂起。这是 MCR 的已知问题：

**临时解决方案**:

```c
// 使用强制终止
#ifdef _WIN32
HANDLE hProcess = GetCurrentProcess();
TerminateProcess(hProcess, 0);
#endif
```

## 总结

MATLAB Library Compiler 为 MATLAB 算法的跨语言集成提供了强大的解决方案。通过合理的配置和正确的使用方法，可以有效地将 MATLAB 的计算能力集成到其他应用程序中。在实际部署时，需要特别注意 MCR 的安装和版本兼容性问题。
