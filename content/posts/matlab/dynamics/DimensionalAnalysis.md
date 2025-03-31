+++
title = 'Dimensional Analysis量纲分析入门'
date = 2025-03-31T08:44:49+08:00
draft = false
mathkatex = true
categories = ['matlab', 'dynamics', 'ode']
tags = ['dimensional analysis', 'dynamics', 'matlab', 'ode45', 'ode', 'uifigure', 'App Designer']
toc = true
tocBorder = true
+++


## 幂律（Power Law）

在物理学中，幂律（Power Law）是指量之间的幂次关系，通常表示为：
$$
y = kx_1^{n_1}x_2^{n_2}\cdots x_m^{n_m}
$$

其中，$k$ 是常数，$x_1, x_2, \cdots, x_m$ 是变量，$n_1, n_2, \cdots, n_m$ 是幂次。

    定理：有单位的物理量，只能形成幂律关系。

这个定理的证明很简单（！）。

![除掉唐僧师徒](/matlab-img/dimensionalanalysis/killit.jfif)

**证明**：不失一般性，假设$y$和$x$是两个有单位的物理量，那么$y$和$x$的关系可以表示为：

$$
y = f(x)
$$

量纲，也就是物理单位，通常本身也是一种比例关系，例如长度，很原始的基准是手肘到中指尖的长度（英尺），很科幻的现代基准是光在真空中1/299792458秒内走过的距离（米）。有物理单位的量，可以通过取比值的方式来消去单位。

$$
\frac{y_1}{y_2} = \frac{f(x_1)}{f(x_2)}
$$

这个式子，就是一个与单位无关的公式，也就是说，无论$x$的单位是什么，$y$的单位是什么，这个式子都成立。

那么这里就有一个决定性的推理。

$$
\frac{f(x_1)}{f(x_2)} = \frac{\alpha x_1}{\alpha x_2}
$$

也就是，$x$取不同的单位，体现为单位转换系数$\alpha$，$\frac{y_1}{y_2}$ 不变。只要这个式子成立，很容易就能得到：$y$和$x$就满足幂律关系。

对上面的式子求取$\alpha$的导数，可以得到：

$$
x_1f(\alpha x_2) f'(\alpha x_1) = x_2 f(\alpha x_1) f'(\alpha x_2)
$$

对所有的$\alpha, x_1, x_2$，这个式子都成立，我们取$\alpha = 1, x_2 = 1, x_1 = x$，可以得到：

$$
\frac{x f'(x)}{f(x)} = \frac{f'(1)}{f(1)} \equiv k
$$

这里的$k$是一个常数。

$$
\int \frac{f'(x)}{f(x)} = \int \frac{k}{x}
$$

计算不定积分，可以得到：

$$
\ln f(x) = k \ln x + c
$$

取指数，可以得到：

$$
f(x) = C x^k
$$

这里$C$是另外一个积分常数。Quod Erat Demonstrandum （Q.E.D.）。

 
当然，从一般的物理规律出发，也能理解为什么有单位的量只能出现在幂律关系中。就比如一个长度量$l$，$e^l$的含义是什么？$\sin(l)$的含义是什么？从数学上来看

$$
e^l = 1 + l + \frac{l^2}{2!} + \frac{l^3}{3!} + \cdots
$$


等于把无量纲量、长度量、面积量、体积量……全部加在一起，这显然是没有任何物理意义的。

## SI量纲

SI量纲，也就是国际单位制，是国际上通用的物理量单位体系。SI量纲有7个基本量，参考国标[GB 3100-1993](https://openstd.samr.gov.cn/bzgk/gb/newGbInfo?hcno=ADAA308A0BE559EC29E773B71591F463)的3.1节SI基本单位：

- 长度（$L$）, 单位：米（$m$）
- 质量（$M$）, 单位：千克（$kg$）
- 时间（$T$）, 单位：秒（$s$）
- 电流（$I$）, 单位：安培（$A$）
- 热力学温度（$Θ$）, 单位：开尔文（$K$）
- 物质的量（$N$）, 单位：摩尔（$mol$）
- 发光强度（$J$）, 单位：坎德拉（$cd$）

我们把一个物理量的单位信息记为$[y]$，表达为上述基本量的幂次形式，例如速度的单位是$\frac{m}{s}$，那么$[v] = L T^{-1}$，加速度的单位是$\frac{m}{s^2}$，那么$[a] = L T^{-2}$。

所以对于有单位物理量$y$，可以表示为：

$$
y = C a^\alpha b^\beta c^\gamma \cdots
$$

其中$a, b, c, \cdots$是有单位的物理参数，$C$是常数，$\alpha, \beta, \gamma, \cdots$是幂次。

$$
[y] = [a]^\alpha [b]^\beta [c]^\gamma \cdots
$$

再结合上面的SI量纲，可以得到一组方程，分别对应7个基本量：

$$\begin{split}
&[y] = L^{\alpha_y} M^{\beta_y} T^{\gamma_y} I^{\delta_y} Θ^{\epsilon_y} N^{\zeta_y} J^{\eta_y} \\
&[a] = L^{\alpha_a} M^{\beta_a} T^{\gamma_a} I^{\delta_a} Θ^{\epsilon_a} N^{\zeta_a} J^{\eta_a} \\
&[b] = L^{\alpha_b} M^{\beta_b} T^{\gamma_b} I^{\delta_b} Θ^{\epsilon_b} N^{\zeta_b} J^{\eta_b} \\
&[c] = L^{\alpha_c} M^{\beta_c} T^{\gamma_c} I^{\delta_c} Θ^{\epsilon_c} N^{\zeta_c} J^{\eta_c} \\
&\cdots
\end{split}
$$

这样可以得到一组方程：

$$\begin{split}
&\alpha_y = \alpha_a^\alpha + \alpha_b^\beta + \alpha_c^\gamma + \cdots \\
&\beta_y = \beta_a^\alpha + \beta_b^\beta + \beta_c^\gamma + \cdots \\
&\gamma_y = \gamma_a^\alpha + \gamma_b^\beta + \gamma_c^\gamma + \cdots \\
&\delta_y = \delta_a^\alpha + \delta_b^\beta + \delta_c^\gamma + \cdots \\
&\epsilon_y = \epsilon_a^\alpha + \epsilon_b^\beta + \epsilon_c^\gamma + \cdots \\
&\zeta_y = \zeta_a^\alpha + \zeta_b^\beta + \zeta_c^\gamma + \cdots \\
&\eta_y = \eta_a^\alpha + \eta_b^\beta + \eta_c^\gamma + \cdots \\
\end{split}
$$

## 举个例子

我们来分析一个单摆的周期。单摆的长度为$l$，质量为$m$，角频率为$\omega$，重力加速度为$g$。

![单摆](/matlab-img/dimensionalanalysis/pendulum.png)

我们写成：

$$
\omega = C l^\alpha m^\beta g^\gamma
$$

量纲分析有：

$$
\begin{split}
&[l] = L \\
&[m] = M \\
&[g] = L T^{-2} \\
&[\omega] = T^{-1}
\end{split}
$$

因此有：

$$
T^{-1} = L^\alpha M^\beta (L T^{-2})^\gamma
$$

$$\left\{
\begin{split}
&\alpha + \gamma = 0 \\
&\beta = 0 \\
& -2\gamma = -1
\end{split}
\right.
$$

解得：

$$
\left\{ 
\begin{split}
&\alpha = -\frac{1}{2} \\
&\beta = 0 \\
&\gamma = \frac{1}{2}
\end{split}
\right.
$$

所以有：

$$
\omega = C l^{-\frac{1}{2}} g^{\frac{1}{2}} = C \sqrt{\frac{g}{l}}
$$

实际上，通过动力学积分，也能够很**简单**地得到：

$$
\omega = \sqrt{\frac{g}{l}} = 2\pi f
$$

频率为
$$
f = \frac{1}{2\pi} \sqrt{\frac{g}{l}}
$$

周期为

$$
T = \frac{1}{f} = 2\pi \sqrt{\frac{l}{g}}
$$

这个很简单的推导就留给读者作为练习（！）。

![你懂我的意思吧](/matlab-img/dimensionalanalysis/known.jfif)

## 进一步的讨论

当然，前面的7个方程，其解的情况可以分为三类：

- 无解：这个是时候，我们就知道肯定丢失了重要的参数
- 唯一解：非常幸运，只需要通过实验确定常数$C$
- 多解：依然得到了一些有用的信息，可以用于指导实验

对于无解和唯一解的情形，我们很容易理解。那么对于多解的情形，物理上的含义如何呢？我们如何选择呢？有什么意义呢？

我们再考虑一个例子，用速度$V$把一个质量为$m$的物体向上抛出，这个球所受到的空气阻力服从平方率，也就是阻力$k V^2$。这里的$k$是阻力系数。问，这个物体上升的高度$h$和什么有关？

$$
h = C m^\alpha g^\beta k^\gamma V^\delta
$$

有$[k] = [force]/[velocity]^2 = M L T^{-2} L^{-2} / (L T^{-1})^2 = M L^{-1}$，所以有：

$$\left\{
\begin{split}
& \alpha + \gamma = 0 \\
& \beta - \gamma + \delta = 1\\
& -2\beta -\delta = 0
\end{split}
\right.
$$

这个方程组有无穷多解。例如前面所说的

$$
\left\{ 
    \begin{split}
        & \alpha = 1 \\
        & \beta = 0 \\
        & \gamma = -1 \\
        & \delta = 0
    \end{split}
\right.
$$

这就表明， 上升高度与$m/k$成正比，如果我们把$h$替换为$h/(m/k)$，就可以得到一个无量纲量。进行同样的量纲分析

$$
\frac{h}{m/k} = C m^\alpha g^\beta k^\gamma V^\delta
$$

得到如下的方程，因为是$h/(m/k)$是无量纲量，所以方程的右边全部是0。

$$\left\{
\begin{split}
& \alpha + \gamma = 0 \\
& \beta - \gamma + \delta = 0\\
& -2\beta -\delta = 0
\end{split}
\right.
$$

解得：

$$
\left\{ 
    \begin{split}
        & \beta = \alpha \\
        & \gamma = -\alpha \\
        & \delta = -2\alpha
    \end{split}
\right.
$$

所以有：

$$
\frac{h}{m/k} = C m^\alpha g^\alpha k^{-\alpha} V^{-2\alpha} = C \left(\frac{mg}{k V^2}\right)^\alpha
$$

从这里我们可以得出结论，$\lambda = \frac{mg}{k V^2}$是我们问题中的唯一需要考虑的无量纲量。

$$
\frac{h}{m/k} = f \left(\frac{mg}{k V^2}\right) \text{, i.e. } h = \frac{m}{k} f \left(\frac{mg}{k V^2}\right)
$$

因为$\lambda$是一个无量纲量，所以这个函数$f$就不见得是幂律关系。实际上，如果我们简单（！）地分析动力学，可以得到：

$$
f(\lambda) = \frac{1}{2} \ln (1+\lambda ^{-1})
$$

推导到这里就有一个问题，因为前面我们选择的组合是随意的，所以的，这个结果有意义吗？这里只是简单展示一下。

例如，

$$\left\{
\begin{split}
& \alpha + \gamma = 0 \\
& \beta - \gamma + \delta = 1\\
& -2\beta -\delta = 0
\end{split}
\right.
$$

的解还可能是：

$$
\left\{ 
    \begin{split}
        & \alpha = 0 \\
        & \beta = -1 \\
        & \gamma = 0 \\
        & \delta = 2
    \end{split}
\right.
$$

这个时候，上面的过程就变成了：

$$
\frac{h}{V^2/g} = C m^\alpha g^\beta k^\gamma V^\delta
$$

当然，这次的量纲方程组求解会得到同样的结果，也就是$\lambda = \frac{mg}{k V^2}$。

$$
h = \frac{V^2}{g} \hat{f} \left(\frac{mg}{k V^2}\right)
$$

观察发现，

$$
\tilde{f}(\lambda) = \hat{f} / \lambda
$$

$$
h = \frac{m}{k} \tilde{f} \left(\frac{mg}{k V^2}\right)
$$

因此，选择不同的解，得到的结果是等价的。

通常，我们也把这个公式写成：

$$
g\left(\frac{mg}{kV^2}, \frac{kh}{m}\right) = 0
$$

这里$g$是一个函数，$\frac{mg}{kV^2}$和$\frac{kh}{m}$是两个无量纲量。当然，$g$可以有很多种用$f$表示的形式。

$$
g(x, y) = y - f(x)
$$

$$
g(x, y) = y^2 - f^2(x)
$$

或者

$$
g(x, y) = \ln \frac{1+y}{1+f(x)}
$$

如果在上面的例子里面，我们再增加变量，就是丢球的角度$\theta$，因为$\theta$是角度，一个无量纲量，那么我们就能写成：

$$
h = \frac{m}{k} F \left(\frac{mg}{k V^2}, \theta\right) = \frac{m}{k} F \left(\lambda, \theta\right)
$$

实际上，$F$无法写出解析形式。

那么，我们这么做有什么意义呢？

通过量纲分析，我们把一个依赖于多个物理量的函数，简化为一个依赖于较少的无量纲量的函数，这非常有助于我们对问题的理解，并且可以指导我们进行实验设计。

## 抛石问题分析

考虑一个二维的抛石问题，定义位置向量为$\vec{x} = (x, y)$，抛石角度为$\theta$，抛石初速度为$V$。

$$
m \ddot{\vec{x}} = m\vec{g} - k |\dot{\vec{x}}| \dot{\vec{x}}
$$

展开得到：

$$
\begin{split}
& m \ddot{x} = -k \sqrt{\dot{x}^2 + \dot{y}^2} \dot{x} \\
& m \ddot{y} = -k \sqrt{\dot{x}^2 + \dot{y}^2} \dot{y} - mg
\end{split}
$$

$t=0$时，$\vec{x} = (0, 0)$，$\dot{\vec{x}} = (V \cos \theta, V \sin \theta)$。

经过上面的量纲分析，我们可以发现，长度的量纲组合是$m/k$，速度的量纲组合是$V$，那么时间量纲组合是$m/kV$。我们就可以按照量纲组合的把量纲量转化成无量纲量。

$$
X = \frac{x}{m/k}, \quad Y = \frac{y}{m/k}, \quad T = \frac{t}{m/kV}
$$

根据链式法则，

$$
\dot{x} = \frac{dT}{dt}\frac{dx}{dT} = \frac{kV}{m} \frac{d}{dT}\left(\frac{m}{k} X\right) = V X'
$$

$$
\ddot{x} = \frac{dT}{dt}\frac{d\dot{x}}{dT} = \frac{kV}{m} \frac{d}{dT}(V X') = \frac{kV^2}{m} X''
$$

最终可以得到：

$$
\begin{split}
& kV^2 X'' = -k \sqrt{V^2 X'^2 + V^2 Y'^2} V X' \\
& kV^2 Y'' = -k \sqrt{V^2 X'^2 + V^2 Y'^2} V Y' - mg
\end{split}
$$

根据$\lambda = \frac{mg}{kV^2}$，可以得到：

$$
\begin{split}
& X'' = - \sqrt{X'^2 + Y'^2} X' \\
& Y'' = - \sqrt{X'^2 + Y'^2} Y' - \lambda
\end{split}
$$

$T = 0$时，$X = 0, Y = 0, X' = \cos \theta, Y' = \sin \theta$。这个方程组的初始值和方程都依赖于无量纲量，整个问题只依赖于两个参数$\lambda$和$\theta$。对于其运动的最高点（对应时间$T_0$），$Y'(T_0) = 0$，此时$h = (m/k) Y(T_0)$。

这就成功地把一个$(m, k, g, V, \theta)$的问题，转化为了一个$(\lambda, \theta)$的求解。我们这里甚至可以给$\lambda$一个名称，考虑到这个参数实际上描述了重力与阻力的相对关系，我们就叫它格拉维-夸德雷特-埃尔雷兹斯唐斯数，简称Grr数。

## 程序实现

都已经写到这里了，很难不搞一个程序。

```matlab
{{% codeseg "static/matlab/dynamics/dimensional/ProjectileApp.m" 507 530 %}}
```

这两个函数定义了ODE和终止条件（高度再次为0的事件）。

调用ODE45求解，得到结果。

```matlab
{{% codeseg "static/matlab/dynamics/dimensional/ProjectileApp.m" 387 404 %}}
```

界面什么的就不纠结了，左边是几个物理参数：质量、阻力系数、重力加速度、初始速度、抛射角度；下面是相应的参考长度、参考时间、最大投射高度（有量纲、无量纲）。

```matlab
app = ProjectileApp;
exportapp(app.Figure, "mainui.png")
```

![界面](/matlab/dynamics/dimensional/mainui.png)

右边两个标签，分别是有量纲的轨迹和两个无量纲参数下最大射程的图形。

我们就演示一个东西，就是不同的质量下的投射距离。我们一通调节质量，得到如下的结果：

```matlab
exportgraphics(app.DimensionalAxes, "ndt.png")
```

![界面](/matlab/dynamics/dimensional/ndt.png)

```matlab
exportgraphics(app.DimensionalAxes, "dt.png")
```

![界面](/matlab/dynamics/dimensional/dt.png)

挺好玩的结论：

- 质量越大，有量纲的投射距离越大，但是无量纲的投射距离越小。
- 质量很大之后，有量纲的投射距离会趋于一个定值，但是无量纲的投射距离会趋于0。

因为参考长度为$m/k$，所以质量越大，参考长度越大，这就是上面现象的原因。

至于其他参数的变化，读者可以自行尝试。


### 完整代码

[完整代码](/matlab/dynamics/dimensional/ProjectileApp.m)

```matlab
{{% codeseg "static/matlab/dynamics/dimensional/ProjectileApp.m" %}}
```


## 总结

量纲分析，通过量纲组合，把一个依赖于多个物理量的函数，简化为一个依赖于较少的无量纲量的函数，这非常有助于我们对问题的理解，并且可以指导我们进行实验设计。