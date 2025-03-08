+++
title = 'QuickCodeAndResults(1)'
date = 2024-12-08T19:19:49+08:00
draft = false
mathkatex = true
categories = ['matlab', 'quickanswer']
tags = ['matlab']
toc = false
tocBorder = true
+++


## results

![](/matlab-code/yellow_for_t1_le_t2.png)

这里的黄色对应与$t_1 < t_2$的情况，即第一个算法的运行时间小于第二个算法的运行时间。

并没有显著的性能差异。


## Matlab Code

用于评估的脚本，最好是把`timeit`的评估对象包装为函数，可以去掉首次调用的时间。

- [jacobiComparison.m](/matlab-code/jacobiComparison.m)


```matlab
{{% codesnap "static/matlab-code/jacobiComparison.m" %}}
```


待评估的核心代码：

- [iter_jacobi.m](/matlab-code/iter_jacobi.m)

```matlab
{{% codesnap "static/matlab-code/iter_jacobi.m" %}}
```


## Data
如果不想运行，也可以直接下载结果，R2023b版本的Matlab。

- [comparison-1.mat](/matlab-code/comparison-1.mat)
- [comparison-2.mat](/matlab-code/comparison-2.mat)
- [comparison-3.mat](/matlab-code/comparison-3.mat)
- [comparison-4.mat](/matlab-code/comparison-4.mat)
- [comparison-5.mat](/matlab-code/comparison-5.mat)
- [comparison-6.mat](/matlab-code/comparison-6.mat)
- [comparison-7.mat](/matlab-code/comparison-7.mat)
- [comparison-8.mat](/matlab-code/comparison-8.mat)
- [comparison-9.mat](/matlab-code/comparison-9.mat)
- [comparison-10.mat](/matlab-code/comparison-10.mat)
- [comparison-11.mat](/matlab-code/comparison-11.mat)
- [comparison-12.mat](/matlab-code/comparison-12.mat)
- [comparison-13.mat](/matlab-code/comparison-13.mat)
- [comparison-14.mat](/matlab-code/comparison-14.mat)
- [comparison-15.mat](/matlab-code/comparison-15.mat)
- [comparison-16.mat](/matlab-code/comparison-16.mat)
- [comparison-17.mat](/matlab-code/comparison-17.mat)
- [comparison-18.mat](/matlab-code/comparison-18.mat)
- [comparison-19.mat](/matlab-code/comparison-19.mat)
- [comparison-20.mat](/matlab-code/comparison-20.mat)