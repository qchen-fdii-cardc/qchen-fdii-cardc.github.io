+++
title = 'Vectors in Matlab中的向量约定'
date = 2024-09-07T10:13:33+08:00
draft = false
mathjax = true
categories = ['matlab']
tags = ['Matlab','编程', '教程','向量', '入门教程', '马特拉波']
toc = true
tocBorder = true
+++


## 前言
MATLAB是一种用于数值计算和数据可视化的高级编程语言。以前，都不好意思说它是编程语言，它实际上只是一个脚本工具，配套了一堆工具箱。比如Simulink，可以开展非常复杂的仿真，还能编译到实时对象去对接DSP、控制器等硬件。所谓的m语言，实际上是被用在里面作为脚本。

我反复重复的一句话: m语言，最初的时候非常简陋；后面慢慢加这加那，看起来也像是一门编程语言，但是它的核心依然是那个Mat-Lab，矩阵实验室。

在我们搞工程计算的人看来，那些花里胡哨的其实都没啥用，还是Fortran老铁实在。所以很多Matalb和M语言中，很多情况都是作为约定的方式存在。这些约定，实际上对于习惯进行矩阵计算来分析问题的工程师而言，是非常有用的。但是对于学习Matlab的新手来说，这些约定可能会让人感到困惑。

在讨论矩阵之前，我们首先讨论向量$v$。

## 向量的产生

> **约定1：向量产生默认产生行向量。**


比如我们产生一个向量的几个基本方式，直接输入数据。

```matlab
v = [1; 2; 3; 4; 5];
% size(v) = [5, 1]
```

这是一个列向量，行向量是这样的：

```matlab 
v = [1, 2, 3, 4, 5];
```

或者

```matlab
v = [1 2 3 4 5];
```

这个时候就有

```matlab
% size(v) = [1, 5]
```

对于上面等距分布的向量，我们可以用`:`来表示。如果用 两个`:`，中间那个数字就是步长。

```matlab
v = 1:5;
% or v = 1:1:5;
% size(v) = [1, 5]
```

还能通过`linspace`来产生等距分布的向量。

```matlab
v = linspace(1, 5, 5);
% size(v) = [1, 5]
```

还能通过`logspace`来产生对数分布的向量。

```matlab
v = logspace(1, 5, 5);
% size(v) = [1, 5]
% v = [10 100 1000 10000 100000]
```

上述几个方法，都是产生行向量：

1. 输入数据时用空格或者逗号分隔；
2. 用`:`产生等距分布的向量；
3. 用`linspace`产生等距分布的向量；
4. 用`logspace`产生对数分布的向量。


此外，还有一些特殊的向量，比如全0向量，全1向量，随机向量等。这几个函数实际上都是针对矩阵，因此产生都需要指定向量的形状（包括行数和列数）。

```matlab
v = zeros(5, 1);
% size(v) = [5, 1]
```

```matlab
v = ones(5, 1);
% size(v) = [5, 1]
```

均匀分布的随机向量：

```matlab
v = rand(5, 1);
% size(v) = [5, 1]
```

正态分布的随机向量：

```matlab
v = randn(5, 1);
% size(v) = [5, 1]
```

为什么呢？为什么这么约定呢？下面就告诉你。

## 向量与for循环

我没有看到行向量和for循环联系起来的具体文档，我从对for的使用中，发现了这个约定。

```matlab
v = [1, 2, 3, 4, 5];
for i = v
    disp(i);
end
```

```matlab
v = [1; 2; 3; 4; 5];
for i = v
    disp(i);
end
```

这两个例子一定要运行一下。很不容易看出差别。

![for遍历列](/matlab-img/for-vec.png)


从这里的例子可以看到，默认的`v`是行向量，把它放到`for`的语法结构里面，按照行来遍历，没输出一个数字，就有一个回车空行；而把列向量代入`for`语法结构，按照列来遍历，直接输出一个列向量。

> **约定2：`for`循环遍历列。**


## 向量计算

> **约定3：向量计算默认利用列向量。**


为什么我们说是向量是列向量呢？

我们看下面的例子：

```matlab
v = [1, 2, 3, 4, 5];
v(:)
```

这个时候，我们会发现，`v(:)`是一个列向量。也就是说，对于一个向量，我们可以通过`(:)`来访问时，总是会得到一个列向量。这对于我们编写代码时，是非常有用的。

比如要计算一个矩阵和一个向量的乘积，我们可以这样：

```matlab
A = rand(5, 5);
v = rand(1, 5); % 故意产生一个行向量
A * v(:)
```

这样，我们就不用担心行向量和列向量的问题，得到在数学意义上正确的结果。

另外，比如计算两个向量的内积，我们可以这样：

```matlab
v1 = rand(5, 1);
v2 = rand(5, 1);
% two hours later, 我们已经忘记了这两个向量的方向
v1(:)' * v2(:)
```

虽然，写成`sum(v1 .* v2)`也是可以的，但是这样写，总感觉我们没有更好的从向量的角度来看待计算，不太符合本洁癖的特殊癖好。向量内积，写成矩阵运算：

$$
v_1^T v_2
$$

这里的两个向量都是列向量。

对于矩阵的运算，为什么一定要默认列向量呢？我觉得这可能和矩阵数学理论有一点点关系。

因为矩阵，从数学的角度来看，是一个线性转换。$A_{m \times n}$是一个线性转换，它的作用是将一个$n$维的向量转换为一个$m$维的向量。

在这个意义上，符合数学的定义，我们应该默认是列向量。这个在函数复合的时候，也很符合直观。

$$ \begin{split}
& (f \circ g) (x) \equiv  f(g(x)) \\
& (A \circ B) (v) \equiv  A * (B * v) 
\end{split}
$$





## 结论

1. 向量产生时，默认产生行向量；
2. 向量计算时，默认利用列向量。
3. 矩阵和向量，都是按列存储和访问的。

> **思考题：Matalb中列向量和行向量的内存存储是怎样的？是否有区别？**