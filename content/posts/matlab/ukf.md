+++
title = 'Unscented Kalman Filter in Matlab无迹卡尔曼滤波'
date = 2024-12-10T20:02:54+08:00
draft = false
mathkatex = true
categories = ['matlab']
tags = ['matlab', 'state estimation', 'ukf', 'system identification']
toc = true
tocBorder = true
+++

## 无迹卡尔曼滤波(Unscented Kalman Filter, UKF)

无迹卡尔曼滤波(Unscented Kalman Filter, UKF)是一种基于卡尔曼滤波的非线性状态估计方法，它通过一种称为无迹变换(Unscented Transformation)的方法，将非线性系统的状态估计问题转化为线性系统的状态估计问题，然后使用卡尔曼滤波器进行状态估计。

这是一种在线方法，也就是说，它会连续地对系统状态进行估计，而不是像批处理方法那样，一次性处理所有的数据。

要定义一个UKF，我们需要定义如下两个函数：

1. 状态转移函数 $f(x, u, t)$
2. 观测函数 $h(x)$

其中，$x$ 是状态向量，$u$ 是控制向量，$t$ 是时间，$f$ 是状态转移函数，$h$ 是观测函数。

提供这两个函数外加噪声的信息，既可以创建一个UKF对象。其后，就可以通过`predict`来预测下个时间步长的状态；在拿到实时数据后，通过`correct`函数来校正状态。

![](/matlab-img/unscentedkalmanfilter.png)

系统的框图如上所示。非线性系统的输入为 $u$，系统噪声为 $w$，系统状态为 $x$，观测值为 $y$， 观测噪声为 $v$。UKF 通过输入 $u$ 和观测值 $y$ 来估计系统状态 $\hat{x}$。


## van der Pol Oscillator

这里我们以 van der Pol Oscillator 为例，来演示如何使用 UKF 进行状态估计。

首先，我们给出 van der Pol Oscillator 的一般形式：

$$
\ddot{x} - \mu (1 - x^2) \dot{x} + x = 0
$$

其中，$\mu$ 是 van der Pol Oscillator 的参数。

这个方程可以被转化为两个一阶微分方程：


\begin{cases}
\dot{x_1} &= x_2 \\\\
\dot{x_2} &= \mu (1 - x_1^2) x_2 - x_1 \\\\
\end{cases}


采用ODE45求解这个方程，我们可以得到 van der Pol Oscillator 的状态变化。
    
```matlab
{{% codesnap "/static/matlab-code/+est/vdp1Ode.m" %}}
```

![](/matlab-img/vdp1Ode.png)


## 观测

假设我们对 van der Pol Oscillator 的状态进行观测，但是观测值是带有噪声的。

\begin{cases}
R = 0.2 \\\\
y_1 = x_1 * (1 +  \mathcal{N}(0, R)) \\\\
\end{cases}


```matlab
{{% codesnap "/static/matlab-code/+est/vdp1Observ.m" %}}
```

![](/matlab-img/vdp1Observ.png)

## UKF

接下来，我们使用 UKF 对 van der Pol Oscillator 的状态进行估计。

```matlab
{{% codesnap "/static/matlab-code/+est/vdp1Ukf.m" %}}
```

经过 UKF 估计后，我们可以得到 van der Pol Oscillator 的状态估计值。

![](/matlab-img/vdp1Ukf.png)


## 进一步分析

当然，我们还可以对新息、协方差等进行分析。这里仅仅展示对一个连续的状态进行UKF估计的过程。


参考：[Van der Pol Equation: Overview, Derivation, and Examination of Solutions](https://nickmcmullen.github.io/m312_project_handout.pdf)