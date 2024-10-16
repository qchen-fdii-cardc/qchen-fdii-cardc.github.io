+++
title = 'FEM_Meshing_in_Matlab工具箱PDE之网格划分'
date = 2024-10-16T12:04:31+08:00
draft = false
mathjax = true
categories = ['matlab', 'FEM']
tags = ['matlab', 'fem', '有限元', '网格']
toc = true
tocBorder = true
+++


- [Matlab FEM系列](https:/www.windtunnel.cn/categories/FEM/)


## PDE工具箱的网格数据

PDE工具箱对2D几何体，采用三角形网格，对于3D几何体采取四面体网格。 在这两种情况下，网格单元的可以采取二次单元也可以采用一次单元（线性）。这两个概念在有限元中间指的都是插值函数的阶次。

需要3个点才能构成二次，2个点就只能构成线性插值。PDE只能选择二次插值和线性插值，不能采用混合插值。

二维问题的三角形网格单元，对应线性插值所需要的3个节点编号方式，以及二次插值所需要6个节点编号，如下图所示。

![三角形线性插值和二次插值](/matlab-img/linear-quadratic.png)

对于三维几何，线性插值和二次插值分别对应4个节点和10个节点，如下图。

![四面体线性插值和二次插值](/matlab-img/3d-linear-quadratic.png)

这里二阶差值所额外增加的节点，总是位于对应两个角点的中间。因此对于一些曲面几何体，中心节点有可能并不在边上或者面内。

PDE用模型俩存储求解问题的相关参数，其中关于网格的数据记录在一个字段`Mesh`中，这个字段是一个FEMesh对象，包括的属性非常清晰和透明。

- `Nodes`：节点坐标矩阵，对应所有点的坐标，因此是一个 $D \times Nn$的数组，$D = 2/3$ 分别对应二维和三维问题，所以，每列就是一个空间点位置
- `Elements`：是一个 $M \times Ne$ 的矩阵，对应节点编号构成元素描述，这里的 $M$ 就是描述元素所需要的节点数目，如果是线性插值，二维对象需要3个点，三维对象需要4个点；如果是二次插值，对应是6个点和10个点，每列就是一个元素
- `MaxElementSize`：一个正整数，对应目标网格元素尺寸，通过元素中最长的边的长度来计算。这个参数可以在产生网格时设置
- `MinElementSize`: 一个正整数，最小的网格元素尺寸，跟上面这个一样，是一个网格质量的参数
- `MeshGradation`: 网格增长比例，是一个严格位于 $[1,2]$ 的值，默认是1.5
- `GeometricOder`： 字符串，插值多项式阶次，只能是 `linear`或者`quadratic`

PDE工具箱的网格数据结构，容易理解也很透明。了解有限元或者有限体积或者任何数值偏微分方程基本知识之后非常容易理解和使用。


## 网格产生

PDE工具箱通过`generateMesh`函数来产生网格，在产生过程中，可以设置各种参数。

### 几何体

依然用[结构静力学分析示例](https:/www.windtunnel.cn/posts/matlab/fem-static/)中的几何体作为例子。


```matlab
model = femodel(AnalysisType="structuralStatic", ...
    Geometry="BracketWithHole.stl");
```

![](/matlab-img/BracketWithHole.png)

这个时候，`model`对象中包含了几何体信息，但是还没有网格信息。

```matlab
>> model.Mesh

ans = 
    []
```

下面就来调用`generateMesh`函数产生这个几何的网格。

### 网格生成

`generateMesh`函数兼容经典的PDE模型，也适用于新的统一模型。
所以，函数的第一个参数可以是一个`PDEModel`对象，也可以是一个`femodel`对象。
唯一的要求是，这个对象必须包含几何体信息。

```matlab
model = generateMesh(model);
pdemesh(model);
```

![](/matlab-img/BracketWithHoleRearMesh.png)

其实这个网格像模像样的，一般而言，我们只需要这样就可以做一点小小的计算工作。

这里位移需要注意的是，`generateMesh`函数不会改变原来的`model`对象，而是返回一个新的对象，所以需要重新赋值才能得到网格信息。

```matlab
>> model.Mesh

ans = 

  FEMesh - 属性:

             Nodes: [3×7780 double]
          Elements: [10×3874 double]
    MaxElementSize: 0.0141
    MinElementSize: 0.0071
     MeshGradation: 1.5000
    GeometricOrder: 'quadratic'
```

从这里可以看到网格数据结构的各个属性，简直就是一个小透明。

`generateMesh`还提供了一些参数，可以用来调整网格的质量。

调用的方式是：

```matlab
model = generateMesh(model, Name1=Value1, Name2=Value2, ...);
```

或者

```matlab
model = generateMesh(model, 'Name1', Value1, 'Name2', Value2, ...);
```

### 全局控制参数
全局参数针对整个网格，对整个几何体生效，可以用来调整网格的质量。这些参数包括：

| 参数             | 说明                                          |
| ---------------- | --------------------------------------------- |
| `GeometricOrder` | 插值多项式阶次，可以是`linear`或者`quadratic` | \ |
| `Hgrad`          | 网格增长比例，一个正实数，通常在1到2之间      |
| `Hmax`           | 最大网格尺寸，一个正整数                      |
| `Hmin`           | 最小网格尺寸，一个正整数                      |


例如，上面我可以看到默认参数的网格最大尺寸是0.0141，最小尺寸是0.0071，增长比例是1.5，插值阶次是二次。我们首先是把最大网格尺寸调整到0.01试试看。

```matlab
model = generateMesh(model, Hmax=0.01);
```

或者

···matlab
model = generateMesh(model, 'Hmax', 0.01);
```

这两种调用方式都是一样的。

```matlab
>> model.Mesh

ans = 

  FEMesh - 属性:

             Nodes: [3×14314 double]
          Elements: [10×7921 double]
    MaxElementSize: 0.0100
    MinElementSize: 0.0050
     MeshGradation: 1.5000
    GeometricOrder: 'quadratic'
```

可以看到，网格的尺寸变小，元素数目增加。

![denser mesh](/matlab-img/hmax-change.png)

对比一下，还是加密很多。

### 局部控制参数

除了全局控制参数，`generateMesh`还提供了局部控制参数，可以对几何体的某些部分进行特殊处理。

这些参数包括：

| 参数      | 说明               |
| --------- | ------------------ |
| `Hface`   | 特定面的期望尺寸   |
| `Hedge`   | 特定边的期望尺寸   |
| `Hvertex` | 特定顶点的期望尺寸 |

这些参数分别选定了几何体的面、边、顶点，可以对这些部分进行特殊处理。设置的语法也比较相似。

首先是特定面的尺寸，这个参数对应的值按照设定对象的编号来设置，构成一个`{}`的cell数组。

例如：

```matlab
model = generateMesh(model, Hface={[9, 10], 0.001});
pdemesh(model, );
```
这样就加密了两个面的网格（编号分别是9，10），这个元胞数组的元素个数必须是偶数，奇数的位置是元素的编号（例如`1`）或者元素编号的数组（`[1,2]`），偶数的元素就是尺寸，计算尺寸的方式也是变得长度。

![face mesh](/matlab-img/hface.png)


至于不同元素的编号，可以通过`pdegplot`函数来查看。

- 面，对应设置'FaceLabels="on"`
- 边，对应设置`'EdgeLabels="on"`
- 顶点，对应设置`'VertexLabels="on"`
  

比如，要看顶点的标签：

```matlab
pdegplot(model, 'VertexLabels="on"');
```

![vertex labels](/matlab-img/vertexlabels.png)

```matlab
model = generateMesh(model, Hvertex={[19,12], 0.001});
pdemesh(model, 'VertexLabels="on"');
```

![vertex mesh](/matlab-img/hvertex.png)


## 网格质量及其它

通过全局参数，可以对整个网格进行调整；按照查看元素标签-设定元素的方式，可以对网格进行局部调整。

PDE工具箱还提供查看网格质量的函数：`meshQuality`(2018az之后的版本)。

函数的语法可以是：

```matlab
Q = meshQuality(mesh)
Q = meshQuality(mesh,elemIDs)
Q = meshQuality(___,"aspect-ratio")
```

对于我们采用的统一模型，可以直接调用：

```matlab
Q = meshQuality(model.Mesh);
```

得到的Q与`model.Mesh.Elements`的列数相同，每个元素对应一个质量值。这个质量值在区间`[0,1]`之间，越接近1，网格质量越好。通过这个函数，可以查看网格的质量，并找到质量比较不好的元素的编号，然后通过局部控制参数进行调整。

```matlab
hist(Q)
```
就能对网格质量有一个直观的认识。

![mesh quality](/matlab-img/quality.png)

进一步，质量低于0.5的元素。

```matlab
badElements = find(Q < 0.5);

pdemesh(model);
hold on
idx = find(Q < 0.5);
pdemesh(model.Mesh.Nodes, ...
    model.Mesh.Elements(:, idx), ...
    FaceColor='red', EdgeColor='red')
```
![bad elements](/matlab-img/red-quality.png)

图中不太明显但是可以看到的红色元素就是质量不好的元素。

## 总结

1. PDE工具箱提供的网格生成函数`generateMesh`最大的优点是非常透明，网格数据结构非常清晰，可以通过`model.Mesh`来查看网格的各种属性。
2. 通过全局控制参数，可以对整个网格进行调整，通过局部控制参数，可以对网格的局部进行调整。
3. 通过`meshQuality`函数，可以查看网格的质量，找到质量不好的元素，然后通过局部控制参数进行调整。
4. PDE的网格工具功能相对单一，但是对于一般的几何体和相对偏理论的问题，已经足够使用。