+++
title = 'Chp04 Vibration Basics'
date = 2025-08-16T08:53:56+08:00
draft = true
mathkatex = true
categories = ['ode']
tags = ['ode']
toc = true
tocBorder = true
+++


## 振动基础

$$
\dot{u} = dot{u} \\
\ddot{u} = - \frac{\mu}{m} u
$$

转换为ode函数：

```matlab
odefun = @(t, y, m, mu)[
    0, 1;
    -mu/m, 0
    ] * y(:);
```
