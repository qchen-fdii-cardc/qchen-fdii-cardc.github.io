+++
title = 'ODE Basics and Solvers in MATLAB中ODE求解器的基本选择'
date = 2025-07-09T12:27:41+08:00
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
