+++
title = 'Optimization 101优化入门'
date = 2025-04-02T16:10:51+08:00
draft = false
mathkatex = true
categories = ['matlab', 'optimization']
tags = ['matlab', 'symbolic', 'Lagrange multiplier', 'syms', 'assume', 'diff',  'subs',  'solve','assert', 'pretty', 'vpa']
toc = true
tocBorder = true
+++

## 一个简单的问题

一个非常简单的问题，如何用最少的材料做一个容积为1m³的圆柱形容器？在把圆柱形容器简化为等厚的金属板并忽略接头的成本和材料耗损可以把问题描述为：一个圆柱体容器，在容积为1m³的条件下，表面积最小，应该如何选择尺寸？

```matlab
app = BucketOptApp;
exportgraphics(app.ContainerAxes, "bucket.png")
```

![bukcet problem](/matlab/optimization/bucket.png)

这个问题的对象（系统）非常简单，是一个静态对象，可以用两个参数来描述，分别是底面半径和高度，这都是很自然的几何参数。

$$
S_p = \{r, h\}
$$

我们要问的问题或者说要求解的问题是一个最小化的问题，当然难点在于还有一个约束。

$$
\begin{split}
\arg\min_{S_p} \quad & A \\
\text{s.t.} \quad & V = 1
\end{split}
$$

通过简单的几何知识，有如下数学模型：

$$\left\{
\begin{split}
&V = \pi r^2 h \\
&A = 2\pi r h + 2 \pi r^2
\end{split}
\right.
$$

## 符号求解

### 简单问题简单解

这个题目相当简单，在约束关系$V=1$中可以求解得到$h=\frac{1}{\pi r^2}$，代入$A$的表达式中，可以得到：

$$
A = 2\pi r \frac{1}{\pi r^2} + 2 \pi r^2= \frac{2}{r} + 2 \pi r^2
$$

这是一个二次函数，类似于一个碗，最小值点很容易通过求解方程$\frac{dA}{dr}=0$得到。

$$
r_\text{min} = \sqrt[3]{\frac{1}{2\pi}} \approx 0.5419
$$

对应的$h_\text{min}$自然可以得到。

$$
h_\text{min} = 2 \sqrt[3]{\frac{1}{2\pi}} = 2 r  \approx 1.0839
$$

### 自行上难度用Matlab

那我们非要用Matlab做怎么整呢？符号运算工具箱还是很好用的（现在）。

```matlab
{{% codeseg  "static/matlab/optimization/symbolic_opt.m" 1 24  %}}
```

首先，我们在第2行到第4行把问题输进去；第7~8行我们做了一个约束，两个几何量都大于零，这是为了去掉那些复数的解。下面这个例子，可以看到约束函数`assume`的作用。

```matlab
    >> syms x; solve(x^3 == 1)
    ans =
                        1
    - (3^(1/2)*1i)/2 - 1/2
    (3^(1/2)*1i)/2 - 1/2
    >> syms r; assume(r>0); solve(r^3 == 1)
    ans =
    1
```

第10行中，我们定义了一个约束。符号工具箱中，用`==`来表示方程中的等于。这在下面描述要求解的方程中也能用得到。

首先我们符号求解$h$（第12-13行）。

```matlab
    >> hr
    hr =
    1/(r^2*pi)
```

然后第14行，把求解得到的$h(r)$代入面积公式，求解极值点（第16行），最后还要验算是否是最小值点（第18行）。

最后，我们把$h_\text{min}$也求出来，并打印在终端。

### Lagrange multiplier a.k.a. 拉格朗日乘子法

当然，对于约束优化问题，还有一个常用的方式就是拉格朗日乘子法。

$$
\begin{split}
\arg\min_{x} \quad & f(x) \\
\text{s.t.} \quad & g(x) = 0
\end{split}
$$

我们利用拉格朗日乘子$\lambda$把问题写为无约束优化：

$$
\arg\min_{x} \quad  f(x) + \lambda g(x)
$$

```matlab
{{% codeseg  "static/matlab/optimization/symbolic_opt.m" 26  %}}
```

我们非常暴力地把约束条件和优化目标写在一起构成一个新的函数：

$$
L(r, h, \lambda) = A(r, h) + \lambda \left[V(r, h) - 1\right]
$$

然后求解方程组：

$$
\begin{cases}
\frac{dL}{dr} = 0 \\
\frac{dL}{dh} = 0 \\
\frac{dL}{d\lambda} = 0
\end{cases}
$$

最后得到结果：

```matlab
    >> result
    result = 
    struct with fields:

            h: 2*(1/(2*pi))^(1/3)
        lambda: -4*pi*(1/(2*pi))^(2/3)
            r: (1/(2*pi))^(1/3)
```

实际上，我甚至能够无聊到编一个界面来演示这个桶的优化的过程。


## 演示App

### 界面

![bukcet problem](/matlab/optimization/bucketopt.png)

在这个界面里，我们可以用滑块来调节容器的底面半径，界面上同步显示容器的形状，和高度、表面积的值。UI右边是一个坐标系，分别绘制高度和面积曲线，调节过程中，红色与蓝色的圈圈表示当前半径对应的值。

```matlab
exportgraphics(app.PlotAxes, "curve.png")
```

![bukcet problem](/matlab/optimization/curve.png)

当然，我们还提供了一个设置显示最优解的按钮。只能说，无聊的人类会干出无聊的事情。

拖动半径滑块，看看当半径变得非常大后，容器的高度会变得非常小，而表面积会变得非常大。

```matlab
exportapp(app.UIFigure, "flat.png")
```

![bukcet problem](/matlab/optimization/flat.png)

### 源代码

- [BucketOptApp.m](/matlab/optimization/BucketOptApp.m)


```matlab
{{% codeseg  "static/matlab/optimization/BucketOptApp.m" %}}
```

## 模型的碎碎念

这么简单的问题，为什么要纠结？通过这个例子，我们可以窥见目前科学和工程技术研究中解决问题的基本思路：建模。

模型：如果研究者$B$用对象$A^*$来解决对象$A$的某个问题，则称$A^*$为$A$的模型。

这里的定义是一个一般的形式定义，使用的也是一般的用语。这里面，研究者$B$就是建模的主体，而某个问题就是特指的特定问题，而非是泛化的问题，通常是分析、设计、控制、优化等。

而关于系统$A$的模型$A^*$，则挺有意思的。

- 模型$A^*$是关于系统$A$仅仅对解决特定问题有用的简化描述；
- 模型并非总是越复杂越好，恰恰相反，最佳模型是1）符合目的（解决问题）的2）最简单模型；
- 简化是建立模型的基本手段；
- 解决具体问题，总是从最简单的模型出发，逐步增加复杂度，直到模型满足解决该问题的要求。

通常，会采用一个称为$\{S,Q,M\}$的元组来描述一个模型，其中$S$是系统，$Q$是问题，$M$是数学描述。

$$
A^* = \{S, Q, M\}
$$

对我们上面的对象，毫无疑问，$Q$是一个优化问题，也就是$Q=\arg\min_{S_p} \quad A$。而这里$S$的数值表达形式：$S_p$，包含了简化的描述系统$S$的参数，也就是$S_p=\{r,h\}$。

而$M$，则是$S$和$Q$的数学描述，也就是$M=\{V,A\}$。

$$
M=\{V,A\}
$$

那么对于简单的优化问题，我们可以采用上面的符号推导的方式来进行求解，对于问题中的约束条件，则可以采用拉格朗日乘子法来进行求解。

对于更加复杂（$S$复杂、$Q$复杂、$M$复杂或者多个条件复杂）的问题，则需要采用更加复杂的优化算法，比如牛顿法、拟牛顿法、共轭梯度法、内点法、外点法等经典方法；或者采用智能优化算法，比如遗传算法、粒子群算法等。


