+++
title = 'PDE三巨头：热传导方程、波动方程和拉普拉斯方程'
date = 2025-01-24T13:59:08+08:00
draft = true
mathjax = true
categories = ['matlab']
tags = ['fea', 'fem', 'matlab', 'math', 'pde', 'heat', 'wave', 'laplace']
toc = true
tocBorder = true
+++

## 偏微分方程 Partial Differential Equation

### ODE和PDE

我学术生涯（如果我有所谓学术生涯）的前半部分，都在玩ODE，也就是常微分方程（组），大部分时候都是ODE+代数方程；人生真的就像是一盒巧克力，到退休突然一头撞进我一直刻意回避的偏微分方程（组）。大概率也是因为退休什么都不在乎了，所以才开始玩PDE。玩过一段时间的野路子，还是回头把《数学物理方程》捡起来准备重新学习一下。以前学《数学物理方程》的时候，Matlab还不是很熟练，当然软件的功能也不如现在强大。现在我的Matlab力已经达到了一个新的高度，可以考虑把《数学物理方程》重新学习一下，或者从数值求解的角度，能够得到一些新的收获。退休混吃等死，干什么不是干呢？

从物理上来看，通常我们在考虑ODE时，一般会考虑一些物理状态随时间**或者**空间某个维度的变化。通常是时间或者时间的等价量。而PDE则是考虑物理状态随时间**和**空间（或者空间的多个维度）的变化。这就是PDE和ODE的区别。PDE与ODE相比，就是维数的区别，也就是自变量个数是一个还是多个。求解$u=f(x)$是ODE，求解$u=f(t, x_1, \ldots)$或者$u=f(x_1, x_2, \ldots)$是PDE。

从数学上来看，ODE的自变量是一个变量，而PDE的自变量是多个变量。

$$
\frac{du}{dt} = f(t, u)
$$

这是典型的ODE，而PDE则是

$$
\frac{\partial u}{\partial t} = f(t, x, u, \frac{\partial u}{\partial x})
$$

从上面的术学符号可以看到，ODE中是导数$\frac{du}{dt}$，而PDE中是偏导数$\frac{\partial u}{\partial t}, \frac{\partial u}{\partial x}$。我们通常把偏导数写成下标的形式。

$$
\begin{aligned}
&u_x = \frac{\partial u}{\partial x}\\\\
&u_{xx} = \frac{\partial^2 u}{\partial x^2}\\\\
&u_{xy} = \frac{\partial^2 u}{\partial x \partial y}
\end{aligned}
$$

通常，考虑到实际情况，我们会忽略求偏导的顺序，比如$u_{xy} = u_{yx}$。

### PDE的分类

按照偏微分的最高阶次，PDE可以分为：

- 一阶偏微分方程，$\frac{\partial u}{\partial t} = f(x, u, t)$
- 二阶偏微分方程，$\frac{\partial^2 u}{\partial t^2} = f(x, u,t, \frac{\partial u}{\partial x})$
- 三阶偏微分方程，$\frac{\partial^3 u}{\partial t^3} = f(x, u,t, \frac{\partial u}{\partial x}, \frac{\partial^2 u}{\partial x^2})$


对于通用形式如下的方程，

$$
a u_{xx} + b u_{xy} + c u_{yy} + d u_x + f u_y + gu = 0
$$

其中$a, b, c, d, f, g$是常数。根据$b^2 - 4ac$的符号，可以将PDE分为三类：

- $b^2 - 4ac > 0$，双曲型
- $b^2 - 4ac = 0$，抛物型
- $b^2 - 4ac < 0$，椭圆型

典型的，三类方程的代表

- 抛物型方程，如热传导方程
- 双曲型方程，如波动方程
- 椭圆型方程，如拉普拉斯方程


我们这里直接跳过一般介绍PDE会介绍的初始条件、边界条件、线性与非线性、分离变量法、特征线法等内容，直接来看看PDE中最为经典的三个方程：热传导方程、波动方程和拉普拉斯方程。

## 经典三方程


### 热传导方程 Heat Equation
温度是一个很有意思的量，对热力学和温度认识不深的时代，温度被当做是物质蕴含的一种特性，是物质的属性。摄氏度按照水的冰点和沸点划分为0摄氏度和100摄氏度。事实上，温度是物体分子热运动的度量，温度越高，分子的热运动越剧烈。在这个意义上，温度的零度应该是绝对零度，也就是分子的热运动停止。绝对零度是-273.15摄氏度。

而物体中高温区域和低温区域之间温度差异会导致分子的热运动从高温区域向低温区域传递，这个过程就是热传导。这个过程中，

热传导方程描述了分子运动在宏观上的表现，当考虑杆的温度分布变化，以位置$x$和时间$t$的温度$u(x, t)$作为因变量，满足如下方程：

$$
u_t = \alpha u_{xx}
$$

这个过程被称为是diffusion，扩散。热传导方程是一个抛物型方程。