+++
title = 'ODE Solvers in MATLAB中ODE求解器的基本选择'
date = 2025-08-11T12:27:41+08:00
draft = true
mathkatex = true
categories = ['ode', 'matlab']
tags = ['ode', 'matlab', 'ODE求解器', 'MATLAB ODE Solvers', 'MATLAB ODE Basics']
toc = true
tocBorder = true
+++


## Ordinary Differential Equations (ODEs)

常微分方程（ODE，Ordinary Differential Equations）一个或者多个变量及其针对单个自变量的导数的方程。ODE在物理、工程、经济学等领域中广泛应用。通常，这一个自变量是时间，当然也可能是空间或其他变量。这个导数的阶数可以是任意的，通常是整数。

一般而言，$y'$表示$y$对自变量$t$的导数，$y''$表示二阶导数，依此类推。

举例，下面是一个二阶的ODE：

$$
y'' = 9y
$$

在为微分方程求解时，一类是初值问题（IVP，Initial Value Problem），即给定初始条件$y(t_0) = y_0$和$y'(t_0) = y'_0$，求解在$t_0$附近的解。通常，用数值方法得到的结果表达在离散的时间点上：

$$
t = [t_0, t_1, \ldots, t_n]\\
y = [y_0, y_1, \ldots, y_n]
$$

另一类是边值问题（BVP，Boundary Value Problem），即给定边界条件$y(t_0) = y_0$和$y(t_n) = y_n$，求解在$t_0$到$t_n$之间的解。

Matlab提供了多种ODE求解器，适用于不同类型的ODE问题。

## ODE分类

Matlab的ODE求解器主要求解如下类型的一阶ODE：

- 显式ODE，形如：$y' = f(t, y)$
- 线性隐式ODE，形如：$M(t, y)y' = f(t, y)$，这里的$M(t, y)$是一个满秩的矩阵，称为质量矩阵。质量矩阵可能依赖时间，也可能依赖状态变量，也可以是一个常数矩阵。这个矩阵中的导数$y'$与质量矩阵形成线性关系，构成隐式ODE。
线性隐式ODE可以转化为显式ODE的形式。$y' = M^{-1}(t, y)f(t, y)$，其中$M^{-1}(t, y)$是质量矩阵的逆矩阵。直接求解线性隐式ODE可以避免直接求逆矩阵的计算，特别是这种计算可能非常复杂或不稳定的情况。
- 如果在某些其工况下，方程组的某个方程中不包含$y'$，则成为微分代数方程（DAE，Differential-Algebraic Equation）。DAE的求解器通常需要更多的计算资源和时间，因为它们需要同时处理代数方程和微分方程。
- 全隐式ODE，形如$f(t, y, y') = 0$，这种形式的ODE通常需要使用更复杂的数值方法进行求解。

## Matlab对几种特殊类型ODE的支持

### 微分方程组

Matlab的ODE求解器可以处理多个变量的微分方程组。对于一个$n$维的微分方程组，可以将其写成向量形式：

$$
\begin{bmatrix}
    y_1' \\
    y_2' \\
    \vdots \\
    y_n'
\end{bmatrix} = \begin{bmatrix}
    f_1(t, y_1, y_2, \ldots, y_n) \\
    f_2(t, y_1, y_2, \ldots, y_n) \\
    \vdots \\
    f_n(t, y_1, y_2, \ldots, y_n)
\end{bmatrix}
$$

写成向量形式后，可以使用Matlab的ODE求解器直接求解。

```matlab
function dydt = odefun(t, y)
    dydt = zeros(2, 1); % 假设有两个变量
    dydt(1) = y(2); % y1' = y2
    dydt(2) = -9 * y(1); % y2' = -9 * y1
end
```

### 高阶ODE

Matlab的ODE求解器主要处理一阶ODE，但高阶ODE可以通过引入新的变量将其转化为一阶ODE组来求解。例如，考虑一个二阶ODE：

$$
y'' = -9y
$$

可以引入一个新的变量$v = y'$，将其转化为一阶ODE组：

$$
\begin{bmatrix}
    y' \\
    v'
\end{bmatrix} = \begin{bmatrix}
    v \\
    -9y
\end{bmatrix}
$$
然后使用Matlab的ODE求解器进行求解。

### 复数ODE

复数ODE可以按照照实部和虚部分别处理的方法进行求解。将复数变量$y = y_r + iy_i$，其中$y_r$是实部，$y_i$是虚部。然后将复数ODE转化为实数ODE组：

$$
y' = f(t, y)
$$

可以转化为：

$$
\begin{bmatrix}
    y_r' \\
    y_i'
\end{bmatrix} = \begin{bmatrix}
    \text{Re}\left(f(t, y_r + iy_i)\right) \\
    \text{Im}\left(f(t, y_r + iy_i)\right)
\end{bmatrix}
$$

例如：

```matlab
function f = complexf(t, y)
    f = y.*t + 2 * i;
end
```

写成ODE函数：

```matlab
function fv = imaginaryODE(t, yv)
    y = yv(1) + 1i * yv(2); % 复数变量
    f = complexf(t, y); % 计算复数函数值
    fv = [real(f); imag(f)]; % 返回实部和虚部
end
```

在求解时，需要将初始条件也转化为实部和虚部：

```matlab
y0 = 1 + i;
yv0 = [real(y0); imag(y0)];
[t, yv] = ode45(@imaginaryODE, [0 1], yv0);
y = yv(:, 1) + 1i * yv(:, 2); % 将结果转化为复数形式
```

## 求解器基本选择

Matlab提供了多种ODE求解器，适用于不同类型的ODE问题。以下是一些常用的ODE求解器及其适用场景。
