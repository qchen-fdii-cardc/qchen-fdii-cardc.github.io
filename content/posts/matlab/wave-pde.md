+++
title = 'Wave_PDE_In_Matlab求解时变波动方程'
date = 2024-11-30T10:48:17+08:00
draft = false
mathkatex = true
categories = ['matlab', 'FEM']
tags = ['matlab', 'FEM', 'Symbolic Math Toolbox', 'PDE Toolbox']
toc = true
tocBorder = true
+++


## 波动方程

这都很正经波传播结果，不要多想……不要乱想……这些个波都很正经。

![](/matlab-img/wave-3.gif)

![](/matlab-img/wave-4.gif)

![](/matlab-img/wave-1.gif)

![](/matlab-img/wave-5.gif)


网格也很正经。

![](/matlab-img/MeshWithEdgeLabels.png)

这是在一个心脏模型上求解时变波动方程的结果。

$$\frac{\partial ^2}{\partial t^2} u\left(t,x,y\right) =\frac{\partial ^2}{\partial x^2} u\left(t,x,y\right)+\frac{\partial ^2}{\partial y^2} u\left(t,x,y\right)$$

问题的边界按照网格图上分为了四个部分，都能够设置边界条件，当然这里是二阶方程，所以设置边界条件是必须设置$u$和$\frac{du}{dt}$。当然作为一个时变方程，还有初值也需要设置。

该怎么求解呢？


## PDE工具箱系数推导

### 偏微分方程的散度形式

偏微分工具箱（Partial Differential equation Toolbox $^{\text{TM}}$）求解的方程形式为：

$$
\mathbf{m} \frac{\partial^2 \mathbf{u}}{\partial t^2} + 
\mathbf{d} \frac{\partial \mathbf{u}}{\partial t} -
\nabla \cdot (\mathbf{c} \otimes \nabla \mathbf{u}) + \mathbf{a} \mathbf{u} = \mathbf{f}
$$

其中，$\mathbf{u}$ 是未知函数，$\mathbf{m}$ 是质量矩阵，$\mathbf{d}$ 是阻尼矩阵，$\mathbf{c}$ 是扩散矩阵，$\mathbf{a}$ 是刚度矩阵，$\mathbf{f}$ 是外力矩阵。

当一个偏微分方程，可以写成上述散度形式（Divergence Form）,即可以采用有限元方法很方便的求解，这里有一个条件就是方程的系数矩阵必须不包含函数的偏微分（可以包括函数、坐标、时间等变量）。

在PDE工具箱中，把方程组描述为散度形式并设定对应的系数矩阵，然后设置边界、初始条件并通过求解器求解。
```matlab
model = createpde()
% ...
specifyCoefficients(model, m=m, d=d, c=c, a=a, f=f)
% ...
```

所以，通常需要进行的一个问题就是，如何把一个偏微分方程、方程组转化为这样的形式。

当然，数学达人很容易通过变换和推导，有些时候需要求解一些代数方程来得到系数矩阵。但是这对于一般的工程师来说，可能就有些困难了。

这个时候就有一个很好用的Matlab工具箱能够派上用场：符号计算工具箱。

符号计算工具箱提供了两个函数：`pdeCoefficents`和`pdeCoefficientsToDouble`来完成这个转化。得到对应的系数矩阵后，就能够调用PDE工具箱进行求解。这个功能需要2021a之后的版本，如果版本更低，只好升级。

### 推导与求解

这个函数`pdfCeofficents`非常简单。

```matlab
>> help pdeCoefficients
--- sym/pdeCoefficients 的帮助 ---

  pdeCoefficients Extract the coefficients of a pde.
     S = pdeCoefficients(EQ, U) extracts the coefficients
     of a symbolic expression EQ that represents a PDE with the dependent
     variable U. The result is a struct S that can be used in the function
     specifyCoefficients of the PDE toolbox.
 
     S = pdeCoefficients(EQ, U, 'Symbolic', true)
     extracts the coefficients and returns them as a struct S of
     symbolic expressions.
 
    Example:
        syms u(x, y)
        S = pdeCoefficients(laplacian(u) - x, [u]);
        model = createpde();
        geometryFromEdges(model,@lshapeg);
        specifyCoefficients(model, S)
 
        creates a pde model for the equation laplacian(u) == x.
        This example requires the PDE Toolbox.
 
    See also pdeCoefficientsToDouble, CREATEPDE, pde.PDEModel.

    sym/pdeCoefficients 的文档
       doc sym/pdeCoefficients
```

这里的例子也给得很清楚：

```matlab
syms u(x, y)
S = pdeCoefficients(laplacian(u) - x, [u]);
model = createpde();
geometryFromEdges(model,@lshapeg);
specifyCoefficients(model, S)
```
首先，要把被积函数的函数关系定义出来，作为符号表达式。接着，就给出被积函数的整体形式作为函数的第一个参数，并把因变量，只有一个可以省略`[]`，作为第二个参数。

函数返回的结果是一个结构体，这个结构体可以直接作为`specifyCoefficients`的参数，当然，这个形式我们知道，跟分开写`key=value`或者，`'key', value,...`的调用形式是一样的，为了更加清晰，甚至可以故意写成：

```matlab
specifyCoefficients(model, 'm', S.m, 'a', S.a, 'c', S.c, 'f', S.f, 'd', S.d)
```

其他，都可以不用仔细说明。

当然，如果这个推导结果表明偏微分方程无法写成散度形式时，会提示：

```matlab
Warning: After extracting m, d, and c, some gradients remain. Writing all remaining terms to f.
```

这个时候，可以通过下面的方式显示表达式`f`的具体形式，因为转换后剩余的部分，全部都被归到`f`这个项中。当`f`还有偏导项时，PDE工具箱就不能求解这个方程。

```matlab
S.f('show')
```


## 全部代码

最后，完整给出代码。

- [wave](/matlab-code/waveInHeart.m)

代码中唯一个好玩一点点的就是最后的gif生成，当目录下已经有文件的时候，就增加一个编号，这样保证gif文件是新的，当`exportgraphics`采用`Append=true`的形式调用时，就会把图形增加到后面，如果是一个已有的gif，就会发生不希望的情况。

最后就是，固定`ColorMap`增加图像动画的一致性，可以通过`hsv(n)`中的`n`来调节图形的精细程度。

```matlab
{{% codesnap "/static/matlab-code/waveInHeart.m" %}}
```
