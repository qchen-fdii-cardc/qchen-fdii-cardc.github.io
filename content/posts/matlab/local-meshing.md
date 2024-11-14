+++
title = 'Local_Meshing_Control_in_MATLAB中控制局部网格划分'
date = 2024-11-14T22:19:40+08:00
draft = false
mathjax = false
categories = ['matlab','FEM']
tags = ['matlab','FEM', 'meshing']
toc = true
tocBorder = true
+++



## 网格划分控制

### 创造几何的方式

前面，我们对创造几何的方法进行了介绍：

- [CSG几何体描述](/posts/matlab/2d-geometry-csg/#2d几何的表示)
- [预定义几何体](/posts/matlab/structure-static/#matlab-pde-toolbox传统形式)中的`multicuboid`函数，还有其他的比如`multicylinder`、`multisphere`等函数
- [几何函数](/posts/matlab/geometry-function/#matlab-pde工具箱的实现)描述2D区域
- [STL文件导入](/posts/matlab/fem-meshing/#几何体)，直接从外部几何文件导入
- 配合CSG、几何函数和预定几何性状，利用`extrude`函数来产生3D几何

### 网格划分控制

在[网格划分局部控制参数](/posts/matlab/fem-meshing/#局部控制参数)一节中，我们介绍了PDE工具箱中的网格划分中针对特定面、边和顶点的控制尺寸参数。

但是实际在应用中，设置参数是按照编号来进行的。因此，控制的局部必然要与几何中的顶点、边和面的构造结合在一起。

这里结合例子来分析如何控制局部网格划分。

## 编辑几何体方法

### 增加顶点的方法

Matlab PDE工具箱提供了`addVertex`函数来增加顶点。

```matlab
model = createpde();

g = importGeometry(model, "Block.stl");

pdegplot(g, "VertexLabels", "on", "FaceAlpha", 0.5);
```

![origin-geometry](/matlab-img/origin-geometry.png)

增加顶点：

```matlab
VertexID = addVertex(g, "Coordinates", [20 0 50]); 
% Add a vertex at (20, 0, 50), and return the vertex ID = 9
pdegplot(g, "VertexLabels", "on", "FaceAlpha", 0.5);
```

![origin-geometry](/matlab-img/origin-geometry-9.png)

可以看到这里增加了一个顶点，这个顶点的编号为9，通过变量`VertexID`来访问。

这个时候，我们就能够通过这个顶点来控制局部网格划分。

```matlab
generateMesh(model, "Hvertex", {VertexID, 0.1});
pdeplot3D(model);
```

![origin-geometry](/matlab-img/origin-geometry-9-meshing.png)

### 增加多个顶点

当然，增加多个顶点也不话下，同样可以通过`addVertex`函数来实现。

```matlab
model = createpde();

g = importGeometry(model, "Block.stl");
V = ([20 0 50; 40 0 50; 60 0 50; 80 0 50]);
VertexIDs = addVertex(g, "Coordinates", V); % Add vertices at (40, 0, 50), (60, 0, 50), and (80, 0, 50), and return the vertex IDs = 10, 11, 12

figure(1);
pdegplot(g, "VertexLabels", "on", "FaceAlpha", 0.5);
exportgraphics(gcf, '../matlab-img/origin-geometry-4p.png')

figure(2);
generateMesh(model, "Hvertex", {VertexIDs, 0.1});
pdeplot3D(model);
exportgraphics(gcf, '../matlab-img/origin-geometry-4p-meshing.png')
```
可以看到增加的四个顶点的编号：

![origin-geometry](/matlab-img/origin-geometry-4p.png)

此时，通过设置尺寸参数就能够控制局部网格划分。

![origin-geometry](/matlab-img/origin-geometry-4p-meshing.png)


## 几何创建方法


### 原始方案

首先，也就是在分析问题的时候，就确定可能会在哪些边界需要加强网格结构，在实现几何体的过程中，实现进行计算域的划分，产生实际的几何体组合，从而暴漏相应的边界。

假设我们要在一块直板的某个部分增加载荷，则事先就要把计算域在该位置增加额外的划分。

```matlab
%creation of the beam
gm = multicuboid(0.3,0.03,0.003);
model = createpde("structural","modal-solid");
model.Geometry = gm;
pdegplot(gm, "EdgeLabels", "on","FaceAlpha",0.5)
```

上面的几何体为一个完整的直板，我们可以通过`pdegplot`函数来查看其边界的编号。但是要在直板中间的某个位置增加网格划分，就必须增加名为`Edge`的实体。

![whole](/matlab-img/plate-geometry.png)

Matlab提供了`addVertex`和`addFace`的函数，但是恰恰没有提供`addEdge`的函数。

思路前面给出了例子，在给定的直线上面增加很多个点，通过`addVertex`函数，在划分网格的时候，通过这些点来增加网格划分。

### 重回几何创建的迭代

或者，我们重新构造几何体，从物理上将计算域按照特殊条件的线划分为不同的区域，然后再进行网格划分。


```matlab
%Create the 3D geometry by extruding two adjacent rectangles
gd = [3 4 -0.15 -0.11 -0.11 -0.15 -0.015 -0.015 .015 .015;
      3 4 -0.11 0.15 0.15 -0.11 -0.015 -0.015 .015 .015]';
dl = decsg(gd);
gm = geometryFromEdges(dl);
gm = extrude(gm, 0.003);
%Create the model and add the geometry
model = createpde("structural", "modal-solid");
model.Geometry = gm;
figure(1);
pdegplot(gm, "EdgeLabels", "on", "FaceLabels", "on")

figure(2);
generateMesh(model, "Hedge", {14, 0.001})
pdeplot3D(model);
```
新的几何提构造：

![whole](/matlab-img/plate-geometry-2.png)

这样我们就能得到一个在直板中间的局部网格划分。

![whole](/matlab-img/plate-geometry-meshing.png)


## 总结

1. 工具箱提供了有限的节点增加方法，通过增加节点来控制局部网格划分。
2. 最好还是在分析之初，或者在迭代过程中回到几何创建的过程中，来额外产生边和面，从而控制局部网格划分。