+++
title = 'Geometry_Function_In_Matlab中描述PDE积分区域的函数法'
date = 2024-11-14T14:54:48+08:00
draft = false
mathjax = true
categories = ['matlab', 'FEM']
tags = ['matlab', 'FEM', 'pde', 'fea']
toc = true
tocBorder = true
+++



## 几何函数

如何描述一个2D的几何形状？如何描述偏微分方程求解中所涉及的2D几何区域？

### 偏微分方程定义域

这个几何形状是PDE求解的区域。也就是说偏微分方程这个区域中取值（初始值和状态值、系数），在区域的边界上设定边界条件。

$$
m \frac{\partial^2 u}{\partial t^2} + d \frac{\partial u}{ \partial t} - \nabla \cdot (c \nabla u) + a u = f
$$

或者，方程组

$$
\mathbf{m} \frac{\partial^2 \mathbf{u}}{\partial t^2} + \mathbf{d} \frac{\partial\mathbf{u}}{\partial t} - \nabla \cdot (\mathbf{c} \otimes \nabla \mathbf{u}) + \mathbf{a} \mathbf{u} = \mathbf{f}
$$

在区域中进行考虑，准确的说是方程中的变量$u$、$\mathbf{u}$定义在区域上，所以$\nabla$也在区域的两个坐标$x, y$上定义。

$$
\nabla = \left[\frac{\partial}{\partial x}, \frac{\partial}{\partial y}\right]^\text{T}
$$

根据偏微分方程的特性，只有单联通区域中的值才会相互影响，这就为描述区域提供了一些约束，这些约束可以大大简化积分区域的描述。

### 区域的假设与推论

1. 2D区域是互不相交的单联通区域的并集 $\mathbb{D}$
2. 不失一般性，假设$\mathbb{D}$为单个单联通区域
3. 一个单联通区域可能把整个的 $\mathbb{R}^2$ 分割为 $n+1$个单联通子区域 $\{D_i\}$，$i=0,1,2,\ldots, n$，不失一般性假设$D_0=\mathbb{D}$
4. 一个单联通区域有若干闭环边界$\{\Omega_i\}$，$i=1,2,\ldots, n$
5. 进一步，可设$D_0$的外边界对应$\Omega_1$，且$D_1, D_2, \ldots, D_n$为互不相交单联通区域，且


$$
\mathbb{D} = D_0 - D_1 - D_2 - \ldots - D_n
$$

这样，我们就把这个2D几何区域简化成一个单联通的区域，这个单联通区域中可能挖去了0个或者若干个单联通区域。只有一阶嵌套。

所以，问题简化为，如何描述一个闭环边界$\Omega$包围的单联通区域$D$？

## 描述函数

### 通用讨论

要描述这样的一个区域，跟描述其闭环边界是同一个意思。$xy$平面的曲线，写成参数方程就是$\left(x(t), y(t)\right)$，还能对$t$有一些约束条件，比如$t \propto s$，这里$s$为曲线的长度坐标。

这是显式的方法，当然还有隐式的方法，比如

$$
a^2x^2 + b^2y^2 = 1
$$

就是一个椭圆曲线。

其实还有一种描述方法就是距离函数，signed distance function。

将区域的内部定义为正向，区域的外部定义为负向，把平面上一个点到曲线的最近距离定义为函数的值：

$$
\theta(x, y) =
    \begin{cases}
        - d_{min} & x, y \text{ outside} \\
        0 &    x,y\text{ on curve} \\
        d_{min} & x,y\text{ inside}
    \end{cases}    
$$

这个描述也有一些很好玩的特性。

### Matlab PDE工具箱的实现



Matlab PDE工具箱的`fegeometry`可以用一个函数句柄（称为`几何函数`）来描述2D形状，对这个函数，Matlab进行了如下的约定：

1. 每一个闭合曲线/区域（称为区域），用至少两个曲线（称为边）来描述，这个曲线集合中的曲线，仅仅能在头、尾除相连；
2. 根据输入参数的个数，函数范围相关的信息来描述区域

几何函数的范围值定义如下：

1. 0个输入参数`n=geomFunc;`：返回边的条数
2. 1个输入参数`d=geomFunc(bs);`：输入为一个数组（长度为$n$），边的编号，返回一个矩阵$4\times n$，每列（4行）代表一个边，起点的参数$t_0$，终点的参数$t_1$，左边区域的标签，右边区域的标签。这里的区域标签就对应着子区域的编号，约定，区域外部的编号为`0`
3. 2个输入参数`[x,y]=geomFunc(bs, s);`：这里的`s`就代表曲线长度的数组（长度为$n$），`bs`对应为边，可以是一个标量（这个标量会广播到每一个`s`），也可以是一个长度为$n$的向量，程序返回对应点的坐标`x`和`y`。


这里唯一有意思的就是左边和右边的定义问题。按照曲线参数方程的定义，一个曲线

$$
[x(t),y(t)]^{\text{T}}, t\in [0, s]
$$

按照参数增长的方向，前进的左边和右边就是这里定义的左和右。因此，曲线的起点和终点的定义是最终影响左、和右的关键。

### 例子

上面的定义很清楚，那么下面就是例子。第一个例子当然是Matlab自带的一个例子，就是一个圆。

```matlab
edit circleg
```

就能看到这个函数的源代码。

```matlab
function [x,y]=circleg(bs,s)
nbs=4; % Number of boundary segments

d=[0 0 0 0   % Start parameter value
   1 1 1 1   % End parameter value
   1 1 1 1   % Left hand region
   0 0 0 0]; % Right hand region

start_angles = [0;pi/2;pi;3*pi/2];
switch nargin
    case 0
        x=nbs; 
    case 1    
        x=d(:,bs);
    case 2
        if isscalar(bs)
            bs = repmat(bs,size(s));
        end
        x = zeros(size(s));
        y = zeros(size(s));
        for i = 1:numel(s)
           segment = bs(i);
           offset = pi/2*s(i);
           angle = start_angles(segment)+offset;
           x(i) = cos(angle);
           y(i) = sin(angle);            
        end
end
```


这里，很容易可以反推出：

1. 由四条曲线来描述一个圆；
2. 每个曲线的参数范围都是从0到1，曲线左边为内部区域，右边为外部区域
3. 这里还用了数组`bs`作为坐标来索引`d`矩阵
4. 最后，在计算坐标位置时，可以看到，四个曲线的起点分别为`0`,`pi/2`,`pi`和 `3pi/2`,然后，每个去线段的长度对应弧度`pi/2`。

那么，如果我们要用两个曲线来描述一个圆，会是什么样的呢？

```matlab
function [x,y]=circleg2(bs,s)

nbs=2; % Number of boundary segments

d=[0 0   % Start parameter value
    1 1   % End parameter value
    1 1   % Left hand region
    0 0]; % Right hand region

start_angles = [0;pi];
switch nargin
    case 0
        x=nbs;
    case 1
        x=d(:,bs);
    case 2
        if isscalar(bs)
            bs = repmat(bs,size(s));
        end
        x = zeros(size(s));
        y = zeros(size(s));
        for i = 1:numel(s)
            segment = bs(i);
            offset = pi*s(i);
            angle = start_angles(segment)+offset;
            x(i) = cos(angle);
            y(i) = sin(angle);
        end
end
```


调用下面的函数输出区域：

结果输出代码：

```matlab
tiledlayout(1, 2);

nexttile;
pdegplot(@circleg,"EdgeLabels","on","FaceLabels","on", "VertexLabels", "on");
set(gca, 'XLim', [-1.1, 1.1], 'YLim', [-1.1, 1.1]); 
title('cirgleg');

nexttile;
pdegplot(@circleg2,"EdgeLabels","on","FaceLabels","on", "VertexLabels", "on");
set(gca, 'XLim', [-1.1, 1.1], 'YLim', [-1.1, 1.1]); 
title('cirgleg2');

exportgraphics(gcf, 'circleg.png')
```

![](/matlab-img/circleg.png)


```matlab
>> fegeometry(@circleg)

ans = 

  fegeometry - 属性:

       NumFaces: 1
       NumEdges: 4
    NumVertices: 4
       NumCells: 0
       Vertices: [4x2 double]
           Mesh: []


>> fegeometry(@circleg2)

ans = 

  fegeometry - 属性:

       NumFaces: 1
       NumEdges: 2
    NumVertices: 3
       NumCells: 0
       Vertices: [3x2 double]
           Mesh: []
```

这两个函数产生的几何体，区别就在于边的条数和节点的个数：

- 4条曲线，对应4个边，4个端点
- 2条曲线，对应2条边，3个端点

我不是很理解的是为什么，4条曲线时，进行了正确的端点合并，而2条曲线时，端点没有正确合并？


```matlab
>> fegeometry(@circleg2).Vertices

ans =

    1.0000         0
   -1.0000    0.0000
    1.0000   -0.0000


>> fegeometry(@circleg).Vertices

ans =

    0.0000    1.0000
   -1.0000    0.0000
   -0.0000   -1.0000
    1.0000   -0.0000
```

## 会影响网格吗？




```matlab
tiledlayout(1,2);

nexttile;
pdeplot(generateMesh(fegeometry(@circleg)).Mesh);
title('circleg');

nexttile;
pdeplot(generateMesh(fegeometry(@circleg2)).Mesh);
title('circleg2'); 

exportgraphics(gcf, 'circleg-circleg2.png')
```

看起来没有什么影响。

![](/matlab-img/circleg-circleg2.png)


当然，还是有一个影响，就是在设定元素的网格尺寸时，过少的边会减少设定的精确性，所以在建立几何条件是，应该充分考虑网格加密的设定问题。


```matlab
tiledlayout(2,2)
nexttile;
g = generateMesh(fegeometry(@circleg2), 'Hvertex', {[1], 0.01})
pdeplot(g.Mesh);
title('circleg2');
nexttile;
g = generateMesh(fegeometry(@circleg2), 'Hvertex', {[2], 0.01})
pdeplot(g.Mesh);
title('circleg2');
nexttile;
g = generateMesh(fegeometry(@circleg2), 'Hvertex', {[3], 0.01})
pdeplot(g.Mesh);
title('circleg2');

nexttile;
g = generateMesh(fegeometry(@circleg2), 'Hedge', {[1], 0.01})
pdeplot(g.Mesh);
title('circleg2');
```

![](/matlab-img/circleg2-mesh.png)

可以看到，在进行加密时，这两个点没有正确归并并没有影响。同时，更多的边能够提供更好的网格加密控制力度，比如这里，就只能对单个边进行加密，而采用`circleg`函数的4边描述法，还能够更加精细地控制加密区域。


```matlab
tiledlayout(2,2)
nexttile;
g = generateMesh(fegeometry(@circleg), 'Hvertex', {[1], 0.01})
pdeplot(g.Mesh);
title('circleg');
nexttile;
g = generateMesh(fegeometry(@circleg), 'Hvertex', {[2], 0.01})
pdeplot(g.Mesh);
title('circleg');
nexttile;
g = generateMesh(fegeometry(@circleg), 'Hvertex', {[3], 0.01})
pdeplot(g.Mesh);
title('circleg');

nexttile;
g = generateMesh(fegeometry(@circleg), 'Hedge', {[1], 0.01})
pdeplot(g.Mesh);
title('circleg');
```

![](/matlab-img/circleg-mesh.png)



## 总结

Matlab PDE工具箱约定了一个显式描述积分区域的方法。上面介绍了基本的单边界单联通简单区域的函数定义方法，并简单探讨了对网格生成的影响。