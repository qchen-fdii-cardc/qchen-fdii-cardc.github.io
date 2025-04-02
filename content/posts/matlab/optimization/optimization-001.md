+++
title = 'Optimization 001优化入门'
date = 2025-04-02T16:10:51+08:00
draft = true
mathkatex = true
categories = ['optimization']
tags = ['optimization']
toc = true
tocBorder = true
+++

## 优化的前置概念



## 一个简单的问题

下面是一个非常简单的问题，如何用最少的材料做一个容积为1m³的圆柱形容器？在把圆柱形容器简化为等厚的金属板并忽略接头的成本和材料耗损可以把问题描述为：一个圆柱体容器，在容积为1m³的条件下，表面积最小，应该如何选择尺寸？

这个问题的系统非常简单，是一个静态的系统，可以用两个参数来描述：

$$
S_p = \{r, h\}
$$

问题为：
$$
\begin{split}
\arg\min_{S_p} \quad & A \\
\text{s.t.} \quad & V = 1
\end{split}
$$

数学模型：

$$\left\{
\begin{split}
&V = \pi r^2 h \\
&A = 2\pi r h + 2 \pi r^2
\end{split}
\right.
$$

## 优化求解步骤

