+++
title = 'Pde_non_uniform_material'
date = 2025-05-22T07:43:03+08:00
draft = true
mathkatex = true
categories = ['matlab']
tags = ['matlab']
toc = true
tocBorder = true
+++

## 如何处理非均匀的材料

在Matlab PDE工具箱中，非均匀材料的处理非常简单。这里可以分两种情况来讨论。第一种情况，计算与可以分为多个区域，每个区域有不同的材料属性。第二种情况，计算域中材料属性是连续变化的。

## 按区域设定材料

我们先假设一个传热的例子，计算域中包含两个区域，每个区域有不同的材料属性。

在进行几何建模时，我们就应该分别建立两个几何域来组成计算区域。

![C1+C2](/matlab/non-uniform-material/heatTransferInNonUniformPod.png)
