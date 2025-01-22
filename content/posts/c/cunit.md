+++
title = 'C语言三分钟：单元测试及C语言的依赖管理'
date = 2025-01-22T16:47:44+08:00
draft = false
mathjax = false
categories = ['c']
tags = ['c', 'unittest', 'dependency']
toc = true
tocBorder = true
+++

## 单元测试

C语言的单元测试工具，大概有以下4个：

- CUnit：比较简单，基本的测试需求
- Check：支持复杂测试的组织
- Unity：极简风格，嵌入式适用
- Google Test：C++风格的测试框架，凑活着能支持C语言

总的来说，C语言因为过于低级，单元测试实现起来也比较麻烦，要提供高级编程语言（特别是带大量反射特性的语言）的那种方便的测试框架，就比较困难。

我所知道的CUnit和Check都是依赖于宏来进行测试定义，整个其实是比较原始的。有一点比较好的方面就是这两个库都是以依赖库的方式存在，对项目的集成至少在平台上是比较方便的。

这里我们先介绍下Linux平台下安装CUnit的方法。

## Linux系列平台下C语言的依赖管理

### 共享链接库

有一次在某乎上讨论依赖管理工具，说着说着就说到了C语言的依赖管理。主要是Python，Rust，Go和Java的依赖管理工具非常成熟，让人还是比较羡慕的。就有人说什么C/C++源代码依赖管理地狱，还有吹微软的那个什么vcpkg。

对于Linux平台来说，c/cpp的依赖管理是非常成熟，非常牛牪犇逼的。因为c深深地植根于Linux的生态中，包括内核都是C语言编写。Linux平台整个有一种扁平的、基于文件的信息化操作理念。操作系统提供核心的功能、众多的共享链接库提供基础的功能、系统提供了一部分应用程序相互组合来完成信息处理的功能。当需要开发新的功能时，Linux的理念就是充分利用现有的功能，不要重复造轮子。这种重用首先是组合现有应用程序、其次是共享链接库。

就比如，Linux的视觉下，Python这的就是一个系统库文件和C语言共享工具库的前端。Linux下面这样的前端相当多，只需要查看一下`swig`的文档。

说回来C语言的依赖管理。所以什么C语言的依赖管理？那不过是操作系统的libxxx管理而已。整个操作系统的功能分为非常松散的组织单元，采用.so（相当于windows动态链接库）来进行功能的复用。

所以在Linux平台下面，通常为了运行一些软件，需要安装一些名为libxxx的包：

```shell
sudo apt-get install libxxx
```
这个时候，通常会把针对当前系统的.so文件下载安装在系统的查询路径之中。

### 开发依赖管理

当然，采用C语言扩展Linux的功能时，我们就会安装一个开发版的库。

```shell
sudo apt-get install libxxx-dev
```

这个时候，如果这个库同样是C语言开发，那么这个库的头文件通常在`/usr/include`目录下，库文件.a和.so在`/usr/lib`目录下。

这样我们就能够在自己的c语言程序中引用这个库的头文件，链接这个库的.a文件，然后编译链接运行。

```C
#include <xxx.h>
```

编译通常是这样的：

```shell
gcc -o myapp myapp.c -lxxx
```

## CUnit示例

### 安装CUnit

通常，我们会一次性安装三个玩意：

```shell
sudo apt-get install libcunit1 libcunit1-dev libcunit1-doc
```

第一个就是共享链接库，第二个是开发库，第三个是文档。

这个文档安装之后，我们就能够通过`info cunit`或者`man cunit`来查看CUnit的文档。这是Linux下面的一个很好的习惯，所有的库都有文档，而且都是通过`info`或者`man`来查看。

一致性真的让人感觉愉悦。

这也就是为什么正经想搞一下编程，还是要用Linux的原因。当然，现在完全用不着去找过一个电脑，安装Linux之类的。毕竟最好的Linux发行版本就是WSL2。

只需要在Windows商店里面安装一个Ubuntu，然后就能够在Windows下面愉快地玩Linux了。

这种扁平的组合方式来构造一个系统，提供更加复杂的信息化功能，这是Linux的核心理念。在我乎上看到很多的问题，感觉都是中文的信息化教育有一个很大的缺口。

上面那个命令运行完，我们就能够直接利用CUnit来进行单元测试了。

### 单元测试的例子

这里给一个非常简单的例子，我们要测试一个函数`add`，这个函数的功能是两个数相加。

- [add_test.c](/cpp/cunit/add_test.c)


```C
{{% codesnap "static/cpp/cunit/add_test.c" %}}
```

这个文件里面，我们首先引入CUnit的头文件。

```C
#include <CUnit/Basic.h>
#include <CUnit/CUnit.h>
```

但是，代码里我故意写成了`a * b`，这样就会出错。

```C
int add(int a, int b)
{
    return a * b;
}
```
按照道理，我应该分开`add`函数的声明和定义（.h文件和.c文件）。这里在单独弄一个add_test.c文件，然后在Makefile里面编译链接。但是这里我只想很快地展示一下CUnit的使用方法。


接下来就是测试函数`test_add`。
```C
void test_add(void)
{
    CU_ASSERT(add(2, 2) == 4);
    CU_ASSERT(add(0, 0) == 0);
    CU_ASSERT(add(1, 1) == 2);
    CU_ASSERT(add(3, 4) == 7);
    CU_ASSERT(add(-1, 1) == 0);
}

```
这个函数里面，我们用`CU_ASSERT`来断言函数的返回值。如果函数返回值和我们预期的不一样，那么这个测试就会失败。

最后，我们在`main`函数里面，初始化CUnit的注册表，添加一个测试套件，添加一个测试用例，运行测试，清理注册表。
```C
int main()
{
    CU_initialize_registry();
    CU_pSuite suite = CU_add_suite("add", 0, 0);
    CU_add_test(suite, "test_add", test_add);
    CU_basic_run_tests();
    CU_cleanup_registry();
    return 0;
}
```

这里的几个套路函数：

- `CU_initialize_registry`：初始化注册表
- `CU_add_suite`：添加一个测试套件，后面两个参数是两个函数指针，分别是初始化和清理函数
- `CU_add_test`：添加一个测试用例
- `CU_basic_run_tests`：运行测试
- `CU_cleanup_registry`：清理注册表

这几个函数是CUnit的基本用法。

### 编译链接

这个时候，我们就可以用gcc来编译链接这个程序了。

```shell
gcc -o add_test add_test.c -lcunit
```

这个时候，我们就能够得到一个可执行文件`add_test`，运行这个文件，就能够看到测试结果。

```shell
./add_test
```

可以看到输出：

```shell
     CUnit - A unit testing framework for C - Version 2.1-3
     http://cunit.sourceforge.net/


Suite add, Test test_add had failures:
    1. cunit-firesmoke.c:14  - add(1, 1) == 2
    2. cunit-firesmoke.c:15  - add(3, 4) == 7
    3. cunit-firesmoke.c:16  - add(-1, 1) == 0

Run Summary:    Type  Total    Ran Passed Failed Inactive
              suites      1      1    n/a      0        0
               tests      1      1      0      1        0
             asserts      5      5      2      3      n/a

Elapsed time =    0.000 seconds
```

看我们的测试断言通过了2/5！看起来，`*`是一个不错的`+`！

当然，我们把`add`函数改成`a + b`，再次编译链接运行，就能够看到测试通过了。

```shell
     CUnit - A unit testing framework for C - Version 2.1-3
     http://cunit.sourceforge.net/



Run Summary:    Type  Total    Ran Passed Failed Inactive
              suites      1      1    n/a      0        0
               tests      1      1      1      0        0
             asserts      5      5      5      0      n/a

Elapsed time =    0.000 seconds
```

## 总结

还是应该用个WSL来体会一下信息化系统的构造和管理。所以，无聊的时候，学习一下C语言还是很好的。