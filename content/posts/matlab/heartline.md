+++
title = '心脏线与圆滚线的Matlab App'
date = 2025-03-08T21:05:45+08:00
draft = false
mathjax = false
mathkatex = true
categories = ['matlab']
tags = ['matlab', 'app', 'heartline', '圆滚线']
toc = true
tocBorder = true
+++


## 心脏线
![心脏线](/matlab-code/heartline/heart_anim_91_263.gif)

心脏线是一种特殊的曲线，因其形状类似心脏而得名。

### 心脏线的数学定义

心脏线有几种等价的数学定义：

#### 极坐标定义

心脏线在极坐标系中最常见的表达式为：

$$
r = a(1 + \cos\theta)
$$

或者等价形式：

$$
r = a(1 - \cos\theta)
$$

其中 $a$ 是一个正常数，表示曲线的大小。

#### 参数方程定义

心脏线也可以用参数方程表示：

$$
\begin{aligned}
x &= a(2\cos t - \cos 2t) \\
y &= a(2\sin t - \sin 2t)
\end{aligned}
$$

其中 $t$ 是参数，取值范围为 $0 \leq t \leq 2\pi$。

#### 几何定义

从几何上看，心脏线是一个圆在平面上绕另一个圆滚动时，圆上一点所形成的轨迹。两个圆的半径相等，圆心距离为两个圆半径之和。

![心脏线](/matlab-code/heartline/heart_anim_0_0.gif)

### 心脏线的性质

1. 心脏线是一个闭合曲线
2. 在极点处有一个尖点（cusp）
3. 曲线的周长为 $8a$
4. 曲线所围面积为 $\frac{3\pi a^2}{2}$

心脏线在数学和物理中有许多应用，包括光学中的反射和折射现象，以及计算机图形学中的曲线设计。

当两个圆的半径取不同值时，可能形成不同的形状（例如肾脏线）。

![肾脏线](/matlab-code/heartline/kidney_0_0.gif)


## APP实现

我们按照圆滚线的几何定义，实现一个GUI程序，用于绘制各种圆滚线。

![GUI](/matlab-code/heartline/heart_anim.png)


### GUI代码

这个GUI的界面设计和GUI的代码都相当的随意和愚蠢，用一个$5 \times 3$的网格布局，把各个用户输入和动画输出随意放置。一个按钮在启动动画，启动时获取各个输入控件的值，然后调用`heartAnim.m`中的函数，绘制动画。

```matlab
{{% codeseg "static/matlab-code/heartline/heartAnim.m" 1 162%}}
```
### 辅助函数

这个函数用于设置坐标系的范围。

```matlab
{{% codeseg "static/matlab-code/heartline/heartAnim.m" 164 167%}}
```

这个函数用于在设置滚动点的滑块运动时，更新对应的角度值，相应的调用函数在`heartAnim.m`中的第41和66行。

```matlab
{{% codeseg "static/matlab-code/heartline/heartAnim.m" 170 173%}}
```

绘制动画的函数是整个程序的核心，

```matlab
{{% codeseg "static/matlab-code/heartline/heartAnim.m" 175 257%}}
```

这个函数根据输入的滚线参数，产生10000个点的位置，然后绘制动画。

这其中调用了一个在坐标系中绘制圆的函数，

```matlab
{{% codeseg "static/matlab-code/heartline/heartAnim.m" 276 318%}}
```

另外一个稍微好玩一点的函数，就是推导一个圆滚线的近似方程，并输出位Latex字符串，设置坐标系的标题。结果非常近似，对于心脏线，近似方程还是有点点靠谱，其它的看看就好……

```matlab
{{% codeseg "static/matlab-code/heartline/heartAnim.m" 259 273%}}
```

### 完整代码

[完整代码下载](/matlab-code/heartline/heartAnim.m)


## 抽象滚线

最后还有一些很抽象的玩意：


![GUI](/matlab-code/heartline/flower_0_0.gif)

![GUI](/matlab-code/heartline/flower2_0_0.gif)
