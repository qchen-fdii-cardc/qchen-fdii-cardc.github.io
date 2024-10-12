+++
title = 'Fem Static in Matlab静力学有限元的例子'
date = 2024-10-12T17:13:42+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'fem', '有限元', '静力学']
toc = true
tocBorder = true
+++


## 刹车变形的求解步骤

本示例展示了如何使用 MATLAB® 软件进行刹车变形分析。
这个例子是Matlab官方PDE工具箱的第一个例子，所需要的数据文件都由Matlab提供，包括CAD模型文件。
### 步骤 1: 导入 CAD 模型
导入 CAD 模型，这里使用的是一个带有孔的支架模型。

```matlab
model = femodel(AnalysisType="structuralStatic", ...
    Geometry="BracketWithHole.stl");
```

这个是Matlab Unified Modeling的新写法，而不是以前pdemodel的写法。
这里的Geometry参数是一个STL文件，这个文件是Matlab提供的，包含了支架的几何形状。

```matlab
figure
pdegplot(model,FaceLabels="on");
view(30,30);
title("Bracket with Face Labels")
exportgraphics(gcf, "BracketWithHole.png", Resolution=600)
```

![](/matlab-img/BracketWithHole.png)

不知道怎么回事，切换到后面的视图，整个图形都变黑了……不知道是什么魔法要素。

```matlab
view(-134, -32)
title("Bracket with Face Labels, Rear View")
exportgraphics(gcf, "BracketWithHoleRear.png", Resolution=600)
```

![](/matlab-img/BracketWithHoleRear.png)

### 步骤 2: 材料属性
设置材料属性。

```matlab
model.MaterialProperties = ...
    materialProperties(YoungsModulus=200e9, ...
    PoissonsRatio=0.3);
```

这也是新的写法，`model`是一个类。

```
>> whos model
  Name       Size            Bytes  Class      Attributes

  model      1x1              5766  femodel
```

### 步骤 3: 产生网格
产生网格，这里使用的是默认的网格参数。Matlab教程里面喜欢把产生网格放在设置边界条件之后，我喜欢放在之前，因为我通常要做边界条件的变化，而网格是基本不变的。

```matlab
model = generateMesh(model);

figure
pdemesh(model);
title("Mesh of the Bracket")
exportgraphics(gcf, "BracketWithHoleRearMesh.png", Resolution=600)
```

![](/matlab-img/BracketWithHoleRearMesh.png)

### 步骤 4: 边界条件和加载条件

model.FaceBC(4) = faceBC(Constraint="fixed");
这里设置了支架的底部固定，不允许移动。这种设置参数的方式其实也兼容原先的参数对的方式，比如
`faceBC('Constraint', 'fixed')`。
这里的`4`是支架的底部的面的编号，这个编号是在`pdegplot`函数中显示的`F4`。

载荷也是一样的设置方式，注意这里载荷是一个向量，方向是z轴负方向，大小是1e4。

```matlab
model.FaceLoad(8) = faceLoad(SurfaceTraction=[0;0;-1e4]);
```

### 步骤 5: 求解模型
求解模型。

```matlab
tic;result = solve(model);toc
```

这样一个静态问题，7780个节点，求解的时间很短，在我的笔记本上大概是1秒左右。

### 步骤 6: 结果可视化

先看看`result`的结构。

```
result =

  StaticStructuralResults - 属性:

      Displacement: [1x1 FEStruct]
            Strain: [1x1 FEStruct]
            Stress: [1x1 FEStruct]
    VonMisesStress: [7780x1 double]
              Mesh: [1x1 FEMesh]
```

`result`是一个结构体，里面包含了位移、应变、应力、等效应力、网格等信息。



z方向的最大位移直接用`max`函数就可以得到。

```matlab
max(abs(result.Displacement.uz))
```

4.4621e-05，也就是0.0446mm，这个位移是在z方向的，也就是垂直于支架底部的方向。

位移大小（位移向量的膜哦不模）的结果的可视化，可以用`pdeplot3D`函数，这个函数是Matlab的PDE工具箱的函数，用来绘制三维的PDE模型的结果。

```matlab
figure
pdeplot3D(result.Mesh, ColorMapData=result.Displacement.uz)
title("Displacement in z-direction")
exportgraphics(gcf, "BracketWithHoleDisplacement-z.png", Resolution=600)
```

![](/matlab-img/BracketWithHoleDisplacement-z.png)

以此类推，可以绘制应力、应变、范氏等效应力等的结果。

```matlab
figure
pdeplot3D(result.Mesh, ColorMapData=result.VonMisesStress)
title("Von Mises Stress")
exportgraphics(gcf, "BracketWithHoleVMS.png", Resolution=600)
```

![](/matlab-img/BracketWithHoleVMS.png)

## 其他可以自己查看的细节

1. `result`结构体的其他属性，比如`Strain`、`Stress`等。
2. `model`结构体的其他属性，比如`MaterialProperties`、`FaceBC`、`FaceLoad`等。
3. `pdeplot3D`函数的其他参数，比如`FaceAlpha`、`EdgeAlpha`等。
4. `generateMesh`函数的其他参数，比如`Hmax`、`Hmin`等。
5. `faceBC`和`faceLoad`函数的其他参数，比如`Temperature`、`Heat`等。

其实比较麻烦的不是这种比较简单的加载，如果加载是一个函数，或者需要增加一个非常量的载荷，就需要自己编一个函数，然后传给`faceLoad`函数。
这个函数的输入参数是位置和状态，输出是载荷的大小。这个函数的编写就需要一些编程的技巧了。

其实也非常简单，现整一个函数`function ret = boundaryFcn(location, state)`，然后一步一步地调试到运行到这个函数，
在函数里面就能看到`location`和`state`的结构，然后就可以写出这个函数了。

基本上，location对应就是几何，三个坐标，或者面的方向，state对应就是状态，统一用`u`表示求解变量，对于静力学问题，就是位移，对于热传递问题，就是温度。


## 总结

1. 总的来说，这个新的Matlab的PDE工具箱的写法更加面向对象，更加符合现代的编程风格，但是对于老的写法，还是兼容的。
2. 求解静力学问题的过程比较清晰，对于处理非常简单相对偏理论的问题，是很容易拿到结果的，并且，各个数据结构相对透明，比如`Mesh`，节点、单元等信息都可以直接访问。
3. 如果要变更边界条件、载荷，来研究灵敏度，也是非常方便的，只需要修改`model`结构体的属性，然后重新求解就可以了。相应的，如果要做一些优化，也是非常方便的。例如，要进行最大变形的控制，上面的例子中就给出了如何从结果中提取最大位移的方法。
4. 从2016b开始STL文件的导入就慢慢变好，2022b开始STEP文件也开始支持，基本上这个工具的可用性就相应变得比较强了。
5. 上面这个例子要运行，大概至少需要2023a版本，最好是2023b版本。