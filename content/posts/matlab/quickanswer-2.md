+++
title = 'QuickCodeAndResults(2)'
date = 2024-12-17T11:32:53+08:00
draft = false
mathjax = true
categories = ['matlab','quickanswer']
tags = ['matlab', 'integral', 'quickanswer']
toc = false
tocBorder = true
+++


## 问题
您好，想咨询求f(x,y,z,w)=xy+yx+zx+x+z的四重积分，其中x的取值范围为[0，Inf]，y的取值范围为[x-2，x]，z的取值范围为[x-2，y]，w的取值范围为[x-2，z+2]的程序


## 回答
这道题看起来非常简单，用手都能算出来，依次按照$w, z, y, z$的方式积分就行。

但是我们更加通用一点。考虑采用符号积分的方式解决。


```matlab
syms f(x, y, z, w)
```


```matlab
f(x, y, z, w) = x * y + y * x + z * x + x + z
```




$f(x, y, z, w) =x+z+2xy+xz$



按照顺序积分下去。


```matlab
fxyz = int(f, w, x-2, z+2)
```




$fxyz(x, y, z) ={\left(z-x+4\right)}{\left(x+z+2xy+xz\right)}$




```matlab
fxy = int(fxyz, z, x-2, y)
```




$fxy(x, y) =\frac{{\left(y-x+2\right)}{\left(-x^3 -7x^2 y+6x^2 +8xy^2 +46xy+12x+2y^2 +8y-16\right)}}{6}$




```matlab
fx = int(fxy, y, x-2, x)
```




$fx(x) =16x^2 -\frac{8x}{3}-\frac{20}{3}$




```matlab
fi = int(fx, x)
```




$fi(x) =-\frac{4x{\left(-4x^2 +x+5\right)}}{3}$



如果是定积分，$[0, \infty]$，结果直接就是$\infty$。


```matlab
fx01 = int(fx, x, 0, 1)
```




$fx01 =-\frac{8}{3}$




```matlab
fx0i = int(fx, x, 0, inf)
```




$fx0i =\infty$


