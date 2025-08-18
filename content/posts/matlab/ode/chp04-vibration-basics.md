+++
title = 'Vibration Basics利用ODE求解研究振动问题'
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
\dot{\begin{bmatrix}
    u                    \\
    \dot{u}
\end{bmatrix}} = \begin{bmatrix}
    \dot{u}                        \\
     - \frac{\mu}{m} u
\end{bmatrix} = \begin{bmatrix}
    0 & 1 \\
    -\frac{\mu}{m} & 0
\end{bmatrix} \begin{bmatrix}
    u \\
    \dot{u}
\end{bmatrix}
$$

转换为ode函数：

```matlab
odefun = @(t, y, m, mu)[
    0, 1;
    -mu/m, 0
    ] * y(:);
```

![初始位置在1.0,速度0.0的振荡](/matlab/Rayleigh-equations/System1D_u0=1.0_v0=0.0.png)

![初始位置在0.0,速度1.0的振荡](/matlab/Rayleigh-equations/System1D_u0=0.0_v0=1.0.png)

![初始位置在1.0,速度0.5的振荡](/matlab/Rayleigh-equations/System1D_u0=1.0_v0=0.5.png)

![初始位置在1.0,速度1.0的振荡](/matlab/Rayleigh-equations/System1D_u0=1.0_v0=1.0.png)
