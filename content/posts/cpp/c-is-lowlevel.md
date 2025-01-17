+++
title = 'C语言三分钟：C真的是又浅又低级'
date = 2025-01-11T20:36:04+08:00
draft = false
mathjax = false
categories = ['cpp']
tags = ['talks', 'low-value', 'cpp', 'c']
toc = true
tocBorder = true
+++

## 讨论C语言是否需要资格

总是看到一些关于C语言的讨论。我C语言花了很多时间学，其实没有怎么用过。因为干的活要么用不到C语言，要么没能力用C语言。

我接触计算机在1995年还是1996年，那个时候只有学校计算机机房有电脑，刚开始我可没学过什么编程，因为还是高中生，那个时候就学打字。

偷偷摸摸地玩那台电脑上的游戏，我记得是字符界面的扫雷还是什么的，后面有什么win 3.1，就有比较好玩的游戏，称为空当接龙。

其后上大学，微机原理、C语言都学挺好的，还做过本科生C语言的辅导老师，具体工作是指（dai）导（xie）大作业。

但是我的确没有找到机会用C语言。

1. 都是微软一边的用电脑，dos-windows。
2. 硬件的事情我有几个师兄做，我基本上在天上飘
3. 等我做系统的时候，都是些cpp的复杂类库上面干的

所以，如果讨论C语言需要资格，我感觉我自己大概是没有什么资格的。

但是，也可能我还是可以阐述一下我个人的看法，毫无价值，也没有意义，就当是梦呓……

## 对C语言的两个认知

### C语言并非设计精良但是到处都是

- 名称：C
- 出生：1970s
- 父亲：[Dennis Ritchie](www.cs.bell-labs.com/~dmr)
- 优势：操作系统（内核、驱动、协议栈）
- 劣势：通用程序设计
- 硬件：超级计算机、微控制器、嵌入式系统

C语言当然比JavaScript还是精良多了。但是C语言既不工程也不科学，只是差不多很敷衍地满足了一些对汇编语言进行薄薄一层封装的要求。当然，赶工的程度比Javascript来说还是优秀很多。但是必须要明白，C语言只提供了非常浅非常薄的一层逻辑抽象。

比如很多通信设备中硬件、微控制器会把I/O针脚或者外设映射到内存中，C语言中可以直接编写这样的程序。

```C
#define UART_BYTE *(char *)0x40008000 
#define UART_SEND *(volatile char *)0x40008001 |= 0x08 

void send_uart(char byte) 
{ 
   UART_BYTE = byte;    // write byte to 0x40008000 address 
   UART_SEND;           // set bit number 4 of address 0x40008001 
}
```
这个函数的两个语句，都是赋值语句……把一个内存地址设为要发送的值，更改发送标志，这个跟汇编语言其实没啥区别……但是C语言提供的这个抽象层有一个很好的隔离、抽象的作用，能够大大提高软件的可移植性（只要大家都这么玩，就能一起愉快地这么玩）。


这就带来两个结果：

1. 强大与灵活，可以直接以硬件和汇编的硬核方式实现所有的功能，没有C语言做不到的，毕竟它提供的是直接操作内存、寄存器的能力；
2. 薛定谔的难度系数，只看语法语义，简单到爆炸，什么复杂的都没有，要做的东西经常复杂到跟运行程序的硬件相当的复杂程度，因为C语言直接面对硬件的复杂度；

风云际会，C语言很快把这个汇编上面一点点的生态位完全占据了，大部分硬件都提供ABI的访问方式，函数调用的参数处理方式也成为了事实标准，这个差不多够用的糟糕玩意儿在如此多年以后已经完全无法摆脱。无论是想干什么，总是会在某个阶段碰到一堆C语言实现的库。

从十年前到现在，写为什么我们离不开C语言的大牛小牛一把一把，我没看过几十篇也看了十几篇。上来就是数落一顿C语言多个简陋多么恶心，接下来就是牛夫人抱怨人家不喊她小甜甜。

最后，C语言的编译器真的到处都有，非常好用，就主打一个成熟，毕竟嘛，M-I-L-F这个懂的都懂……

### 学“会”C语言只要几小时

学会C语言可能真的只需要数小时到数天，因为实在太简单，前提呢，就是学过微机原理。

C语言的全部知识：

- 基础
    - 变量和数据类型
    - 判断语句构造
    - 循环语句构造
    - switch分支结构
    - 函数、头文件与源文件
- 高级内容
    - 指针
    - 结构体
    - 数组
    - 字符串
- 进阶内容
    - 文件和I/O
    - 变长函数参数
    - 类型转换
    - 主函数的命令行参数
    - 递归调用与堆栈溢出
    - 链表、树和常用数据结构

每部分的内容都非常少……

[C99的标准](https://www.dii.uchile.cl/~daespino/files/Iso_C_1999_definition.pdf)，介绍语法和语义的，页码范围29~164，一共足足135页。后面就是标准库，正文部分就400页。

头文件就这么点，如果我没有数错的话，足足24个文件……

![](/cpp/c-is-lowlevel/standar_headers.png)

这个文档写得超级啰嗦，如果不啰嗦的话，[C programming language](http://s3-us-west-2.amazonaws.com/belllabs-microsite-dritchie/cbook/index.html)这个是C语言之父所写那本书的主页（人已经去了，网站好像是bell还是出版社在维护来着），书在这里可以下载。这是16开本的，只有不到四百页。讲C语言的大概200来页，背也能背下来。

- [C programming language](https://usuaris.tinet.cat/bertolin/pdfs/c_programming_language.pdf)


推荐去[Cprogramming.com](https://www.cprogramming.com/tutorial/c-tutorial.html?inl=nv)随便看看，几下学会。

### C语言的代码库

几乎所有的操作系统核心、数据库都采用C语言开发，有些在后期增加C++。为什么？因为机器语言和汇编语言太难用。

- Unix：1972年，C语言重写
- Oracle：1983年，C语言重写
- Windows 1.0：1985前，C语言
- Linux 内核：1991年，C语言


## 几个最近的帖子

### 为什么人们还在使用C

[2023 Why are people still using C](https://thecodist.com/why-are-people-still-using-c/)

- 维护C语言代码之外，不应该再用C从头编东西？
- 需要自己管理所有的内存，太容易出现安全问题
- 选择非常多，不同的任务可以用不同的语言完成
- 为了自己的头发，找一个更加合适的语言
- 学起来非常简单


### 50年后，C为什么还重要

[2024 Why C Remains a Relevant Programming Language After 50 Years](https://blogs.dal.ca/openthink/why-c-remains-a-relevant-programming-language-after-50-years/)

- 效率和速度
- 可移植性
- 工业代码基础设施
- 嵌入式与IoT
- 成熟的生态系统
- 现代语言的基石
- 高性能计算
- 控制与灵活性


### 50年后，C为啥还那么流行？

[Why C Remains Popular After 50 Years?](https://cppdepend.com/blog/why-c-remains-popular-after-years/)

- 语言稳定性：用[微软word1.1的代码](http://www.computerhistory.org/atchm/microsoft-word-for-windows-1-1a-source-code/)为例
- 基础的机制：模块化、封装

### 这么多年以后，世界依然由C语言驱动

[After All These Years, the World Is Still Powered by C Programming](https://www.toptal.com/c/after-all-these-years-the-world-is-still-powered-by-c-programming)

- 操作系统内核依然是古早的C代码或者重写的C代码
- 嵌入式和日常生活被C语言包围
- 先进的程序设计语言，用C语言作为公共接口（ABI）
- 依然还有有趣的C语言项目（！！！）
