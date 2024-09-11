+++
title = 'Matlab中的赋值约定：左值和右值约定'
date = 2024-09-11T21:19:58+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'programming', '编程', '入门', '教程', '左值', '右值']
toc = true
tocBorder = true
+++




## 左值和右值

我们在把Matlab作为计算器来使用时，有些时候会直接列出一个表达式，等着（通常可以忽略等待时间）Matlab打印出计算结果。就这个简单的举动，在计算机科学中，也有一个高大上的名字，叫什么REPL（Read-Eval-Print Loop）。

你们以后如果在拿Matlab作为计算器使用，别人问你们在干什么，你们就可以说，我在利用Matlab的REPL功能进行快速原型开发与验证。

我在好多本书里看到对REPL的意义的解释，基本上都是这样的：REPL是一种交互式编程环境，它可以让你快速地输入表达式，然后立即看到计算结果。这种交互式的编程环境，可以让你快速地验证你的想法，快速地进行原型开发。关于REPL的意义，大厨们简直能写一本书，哦不，几本书。

但是作为一个计算器，Matlab最强大的不是能算一些数，它还能把中间的计算结果保存下来，然后再利用这些计算结果进行下一步的计算。这就涉及到了一个概念，赋值。其他编程语言中，赋值通常包括两个部分：变量申明，变量赋值。

在Matalb中，赋值就是一个动作，把等式左边两设定为等式右边的值，这两个部分叫做左值和右值。就像下面的例子，`a = 1`，`a`是左值，`1`是右值。

```matlab
a = 1;
b = 2;

c = a + b;
```

在Matlab这样的语言中，变量不需要声明，变量的**类型**也不固定，虽然每个变量在赋值之后的确有一个类型。在Matlab中，变量的大小和类型信息可以用`whos`命令查看。

```matlab
whos
```


    >> whos
      Name      Size            Bytes  Class     Attributes

      a         1x1                 8  double              
      b         1x1                 8  double              
      c         1x1                 8  double              

这跟我们在草稿纸上进行数字计算的时候，把中间结果写在草稿纸上，然后再利用这些中间结果进行下一步计算是一样的。只不过，我们通常会是这样。

![等式](/matlab-img/eq.png)


高级计算器Matlab在REPL中提供的这种赋值语句，实际上一种合并了声明**变量**和设定**变量**的值的功能。有时候，这样的一次赋值，即设定变量值的语句也称为表达式。在Matlab中，表达式的一般形式是：

```matlab
左值 = 右值
```

从这里，就可以定义左值和右值。

- 左值：左值是一个变量，它是一个存储位置，用来存储右值的计算结果。
- 右值：右值是一个表达式，它是一个计算过程，用来计算出一个值。

## 作为左值的变量

那么那些东西可以作为左值呢？这就设计到另外一个概念，类型。在一个强类型的编程语言（计算环境）中，一个左值的类型是一个重要的概念，通过类型才能确定变量的存储位置的大小和存储方式。

从我们前面的`whos`命令的输出可以看到，Matlab中的变量有一个属性叫做`Class`，这个属性就是变量的类型。

那么，Matlab中的变量的类型有哪些呢？通过`class`函数可以查看一个变量的类型。查看`help class`可以看到Matlab中的数据类型。

    >> help class
     class  Return class name of object.
        S = class(OBJ) returns the name of the class of     object OBJ.
      
        Possibilities are:
          double          -- Double precision floating     point number array
                             (this is the traditional     MATLAB matrix or array)
          single          -- Single precision floating     point number array
          logical         -- Logical array
          char            -- Character array
          cell            -- Cell array
          struct          -- Structure array
          function_handle -- Function Handle
          int8            -- 8-bit signed integer array
          uint8           -- 8-bit unsigned integer array
          int16           -- 16-bit signed integer array
          uint16          -- 16-bit unsigned integer array
          int32           -- 32-bit signed integer array
          uint32          -- 32-bit unsigned integer array
          int64           -- 64-bit signed integer array
          uint64          -- 64-bit unsigned integer array
          <class_name>    -- MATLAB class name for MATLAB     objects
          <java_class>    -- Java class name for java     objects

从这里可以看到Matlab的基本数据类型。那么问题来了，作为左值的变量的类型从哪里来？

一个明显的来源就是从设置变量值的表达式中的右值的类型来决定，`a = 1`中`1`的类型是`double`，所以`a`的类型也是`double`。那么还有没有其他决定变量类型的方法呢？那就是Matlab的索引过程中的约定。

这里还有一个很奇怪的现象，就是Matlab没有数组或者矩阵类型。

> **约定：Matlab中的所有基础变量都是数组，最平常的就是`size`为1的数组。**

    >> cellfun(@class, {1, [1], [1,1], [1,1;1,1]}, 'UniformOutput', false)
    
    ans =
    
      1×4 cell 数组
    
        {'double'}    {'double'}    {'double'}    {'double'}

这个例子可以看到，这几个变量的类型都是`double`，他们的不同在于`size`。


    >> cellfun(@size, {1, [1], [1,1], [1,1;1,1]}, 'UniformOutput', false)
    
    ans =
    
      1×4 cell 数组
    
        {[1 1]}    {[1 1]}    {[1 2]}    {[2 2]}

这里还可以看到，`1`和`[1]`是同一个东西。

    >> 1 == [1]
    
    ans =
    
      logical
    
       1

这就是为什么Matlab的名字是Matrix Laboratory的原因，所有的基础变量都是矩阵，都作为数组来处理。

## 作为左值的数组

当所有基础类型（数值、字符、逻辑）都是数组的时候，数组也可以作为左值。这个时候，前面我们所讲的索引的各个约定就有非常重要的作用了。

我们可以通过索引，把一个数组的一部分作为左值来赋值。这个实际上不是一个很简单的操作。


```matlab
A = magic(3);

A(1,1) = 1; % 单个元素的左值

A(1,:) = 1; % 一行的左值

A(:,1) = 1; % 一列的左值

A(1:2,1:2) = 1; % 一个矩形区域的左值
```

这里的`A(1,1)`、`A(1,:)`、`A(:,1)`、`A(1:2,1:2)`都是左值，它们都是数组的一部分。这里还有一个并不简单的事情，就是这四个表达式的右边的值都是`1`，但是它们的大小是不同的。这就是Matlab的广播约定。

> **约定：当一个表达式的左值`numel`大于`1`，右值是`size`为`[1,1]`，右值会被拷贝到左值的每一个元素，称为广播。**

对于上面这个例子中的矩阵索引构成的左值，是Matlab中的一个非常重要的特性。在这个赋值过程中，有两种情况：

1. 右值的`numel`等于左值的`numel`，这个时候，右值会被依次拷贝到左值，这个顺序我猜测是按照`sub2ind`的顺序。
2. 右值的`numel`等于`1`，这个时候，右值会被广播到左值的每一个元素。

## 作为右值的表达式

Matalb表达式的右值，归根揭底就只有3种形式：

1. 字面常量，比如`1`、`'a'`、`true`、`[1,2,3]`。
2. 矩阵索引，比如`A(1,1)`、`A(1,:)`、`A(:,1)`、`A(1:2,1:2)`。
3. 函数调用，比如`sin(1)`、`magic(3)`、ones(3,2)`。

那我们平常在Matlab中写的表达式，比如`a + b`、`A * B`中的`+`和`*`是什么呢？只需要在Matlab中输入`help +`就能看到`+`的帮助文档。

    >> help +
     +   Plus.
        A + B adds arrays A and B. A and B must have the same size unless one is a scalar.
        A scalar can be added to a matrix of any size.
     
        C = PLUS(A,B) is called for the syntax 'A + B' when A or B is an object.
     
        See also: plus, minus, uplus, uminus, mtimes, times, rdivide, ldivide, power.

所有的运算符都是函数，这些函数都是Matlab的内置函数。

## 结论

1. Matlab提供的RPEL交互式编程环境，可以快速验证想法，非常适合原型开发。
2. Matlab中的所有基础变量都是数组，最平常的就是`size`为1的数组。
3. Matlab中的变量的类型是通过右值的类型或者矩阵索引来决定的。
4. 广播是Matlab中的一个重要特性，就是把一个标量广播到一个数组的每一个元素。
