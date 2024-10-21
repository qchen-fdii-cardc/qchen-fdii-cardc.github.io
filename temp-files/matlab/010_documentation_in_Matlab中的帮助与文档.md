# Matlab中的帮助与文档

## 1. 前言

一眨眼已经写了十篇文章。

1. [000在Matlab中使用Python包CoolProp](https://blog.csdn.net/withstand/article/details/136862296)
1. [001Matlab运行时间测试与时间复杂度分析](https://blog.csdn.net/withstand/article/details/136944992)
1. [002避免使用for循环](https://blog.csdn.net/withstand/article/details/136944992)
1. [003Matlab中的向量约定](https://blog.csdn.net/withstand/article/details/137040229)
1. [004Matlab中的矩阵约定](https://blog.csdn.net/withstand/article/details/137048522)
1. [005Matlab中的数组索引](https://blog.csdn.net/withstand/article/details/137062079)
1. [006Matlab中的逻辑数组索引](https://blog.csdn.net/withstand/article/details/137075131)
1. [007Matlab学习的启动与加速](https://blog.csdn.net/withstand/article/details/137081753)
1. [008Matlab中的函数约定](https://blog.csdn.net/withstand/article/details/137111570)
1. [009Matlab中的左值和右值约定](https://blog.csdn.net/withstand/article/details/137131662)

写第一篇和第二篇的时候就是随手而为，写完这两篇我就想干脆写个300篇算。

在007（第8篇）里，我已经觉得可能需要梳理一下到底要写什么，主要的想法就是，怎么才能凑300篇呢？现在已经放飞自我，感觉300篇也不是什么难事。

如果一个刚上大学，没有接触计算机的同学把这几篇看完，应该已经能够用Matlab进行一些简单的计算，甚至能够编一个小程序来解决一些真正的问题。但是所有这些帖子，归根结底还是授人以鱼，要真正授人以渔，还是应该好好学习Matlab的帮助与文档。

## 2. Matlab的帮助与文档

### 2.1 Matlab的帮助安装

Matlab的最新版帮助文档有一个在线的版本，可以在MathWorks的网站上查看。以前有段时间Mathworks还搞点必须注册登录才能看之类的，现在好像又没有了。Matlab能够得到这么广泛的使用，其中一个原因就是Matlab的帮助文档写得很好。在Matlab之前，代数计算的openblas一直是存在的，GNU还有一个C版本的科学计算库，GSL（其实文档也不错）。Matlab最好的就是提供了一个比较直观的DSL（M语言，虽然简陋，但是有用），并且提供了一个很好的帮助文档。

Matlab的帮助文档也可以安装在本地，这样可以在没有网络的情况下查看帮助文档。旧版本的我已经忘记是怎么装的了，最新版的Matlab可以直接在菜单里选择把文档安装到本地。如果是下载的DVD版本的Matlab 文档，把那个ISO挂在到虚拟光驱上，在虚拟光驱里的`bin/win64`目录下有一个命令行程序`mpm`，可以用这个程序安装Matlab的帮助文档。

```shell
mpm install-doc --matlabroot="C:\Program Files\MATLAB\R2023b"
```

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/1c867f81695a41e91b83e221ceed5be9.png#pic_center)


### 2.2 Matlab的帮助文档查看

Matlab帮助文档可以通过菜单的帮助按钮来访问，然后就是一个跟电子书差不多的东西，左边可以选不同的内容，右上角的搜索框可以全文搜索。

不过我们一般也可以采用命令行的方式来查看文档。

跟文档相关的命令行函数有以下几个。

- `help`：查看函数的帮助
- `doc`：打开帮助文档
- `docsearch`：搜索帮助文档

还有一个可以搜索所有的函数文件的全文内容的命令`lookfor`，以及查找命令对应函数文件的`which`命令。

**下面我们应该干的第一件事情，就是查看一下`help`命令的帮助。**

## 3. `help`命令

`help`命令是Matlab中最常用的命令之一，它可以查看函数的帮助文档。

```matlab
help help
```

这里其实有一个很好玩的约定，就是`help help`命令到底是什么？

> **约定：当采用不带括号的方式调用函数时，Matlab会把后面的内容当做字符串处理。**

也就是`help help`实际上就是调用`help('help')`函数。

那么还有第二个问题，`help('help')`函数返回的到底是什么？

关于这个问题，请运行以下代码。

```matlab
edit help
```

这个等效于

```matlab
edit('help')
```

或者

```matlab
open('help')
```

**请一定要自己试试看。**

这个命令会打开`help`函数的源代码，这个源代码是一个M文件，可以看到这个文件里面有一些`%`开头的注释，这些注释就是`help`函数的帮助文档。

> **约定：Matlab的帮助文档是通过在函数文件的开头添加注释来实现的。**

这部分的内容包括从函数定义`function`语句之后第一个注释开始，到下一个注释之间的内容。也就是接下来所有连续的`%`开头的行。

自己编写自己的函数式，也最好能够遵循这个约定，当然，首先应该是多打开你感兴趣的函数，看看到底是怎么写的注释，如果你愿意仔细去看函数是怎么写的，那么你就已经是一个超有好奇心的天才Matlab程序员了。

当然我们可以看到，有些函数的源文件，就只有注释，没有实际的代码，这就是一个纯粹的文档文件。函数可能是一个MEX文件，或者是一个内置函数，这些函数的源代码是不可见的。

## 4. 作为报错的帮助

另外，Matlab中的报错信息也是一个很好的帮助文档。当你在Matlab中输入一个错误的命令时，Matlab会返回一个错误信息，这个错误信息是一个很好的帮助文档。

```matlab
a = 1;
a(1,2)
```

    >> a(1,2)
    位置 2 处的索引超出数组边界(不能超出 1)。

不过Matlab没有那种直接返回错误信息和`Exception`的功能。一般只能

```matlab
try
    a(1,2)
catch ME
    disp(ME)
end
```

    MException - 属性:
  
      identifier: 'MATLAB:badsubscript'
         message: '位置 2 处的索引超出数组边界(不能超出 1)。'
           cause: {}
           stack: [0×1 struct]
      Correction: []

可以看到，这个错误是一个`MATLAB:badsubscript`错误，这个错误的信息是`位置 2 处的索引超出数组边界(不能超出 1)。`。

## 5. 结论

1. Matlab的帮助文档是一个很好的学习工具；
2. 可以通过`help`命令查看函数的内置帮助文档；
3. Matlab的内置帮助文档是通过在函数文件的开头添加注释来实现的；
4. Matlab的报错信息也是一个很好的帮助文档。


