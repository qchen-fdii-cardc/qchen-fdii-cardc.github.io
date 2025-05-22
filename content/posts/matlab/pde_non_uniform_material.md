+++
title = 'Matlab PDE工具箱非均匀材料的处理方式'
date = 2025-05-22T07:43:03+08:00
draft = false
mathkatex = true
categories = ['matlab', 'fem']
tags = ['matlab', 'pde', 'non-uniform-material', 'heat transfer', 'material properties']
toc = true
tocBorder = true
+++

## 如何处理非均匀的材料

在Matlab PDE工具箱中，非均匀材料的处理非常简单。这里可以分两种情况来讨论。第一种情况，计算与可以分为多个区域，每个区域有不同的材料属性。第二种情况，计算域中材料属性是连续变化的。

## 按区域设定材料

我们先假设一个传热的例子，计算域中包含两个区域，每个区域有不同的材料属性。

在进行几何建模时，我们就应该分别建立两个几何域来组成计算区域。

![C1+C2](/matlab/non-uniform-material/heatTransferInNonUniformPod.png)

```matlab
{{% codeseg "/static/matlab/non-uniform-material/heatTransferInNonUniformPod.m" 6 13 %}}
```

从这里可以看到，两个Cell（分别是C1和C2）组成了整个计算域。

在材料建模时，我们分别对C1和C2进行材料建模。

```matlab
{{% codeseg "/static/matlab/non-uniform-material/heatTransferInNonUniformPod.m" 21 26 %}}
```

在设置完之后，我们可以查看材料属性。

```matlab
>> model.MaterialProperties
ans = 
  1x2 materialProperties array

Properties for analysis type: thermalTransient

Index    ThermalConductivity    MassDensity    SpecificHeat
  1               1                  1              1      
  2              100                 1              1
```

从这里可以看到，C1和C2的材料属性分别被设置为1和100。

最后，按照正常步骤求解即可。

![result](/matlab/non-uniform-material/result.png)

```matlab
{{% codeseg "/static/matlab/non-uniform-material/heatTransferInNonUniformPod.m"%}}
```

## 设置为连续变化的材料

当然还有一种办法，就是采用材料函数来设定，用这个方法，我们在建模时就不需要分别建立几何域了。

```matlab
{{% codeseg "/static/matlab/non-uniform-material/heatTransferInNonUniformPod2.m" 6 13 %}}
```

![result](/matlab/non-uniform-material/singleCell.png)

我们的材料属性函数（准确来说是材料导热系数）是一个函数句柄，这个函数句柄的输入参数是位置和状态，输出参数是材料导热系数。

```matlab
{{% codeseg "/static/matlab/non-uniform-material/heatTransferInNonUniformPod2.m" 50 52 %}}
```

这就是一个1.0到100的线性函数。

![result](/matlab/non-uniform-material/result2.png)

最后的计算结果跟两段材料大相径庭。全部代码：

```matlab
{{% codeseg "/static/matlab/non-uniform-material/heatTransferInNonUniformPod2.m"%}}
```

如果我们算一个热导率全部取为100的，结果会怎么样？

![result](/matlab/non-uniform-material/result3.png)

只能说，传热还是有那么一点点意思的……

## 总结

我优先会选择按照多个域方式来进行处理，实在不行采取采取第二种方式，第二种方式的计算可能会不收敛，需要精心处理网格和时间步长。

当然这个方法实际上可以用于所有的边界条件、初始条件、材料属性。

对于大部分参数，通用的方式就是：

```matlab
function val = myfun(location, state)
    val = ...;
end
```

而对于初始温度、初始位移或者初始速度，函数变为：

```matlab
function val = myfun(location)
    val = ...;
end
```

这个函数的输入阐述通常是一个结构体数组，可以自行探索在不同的计算中，具体的结构体数组是什么样的。
