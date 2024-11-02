+++
title = 'Structure_Static_PDEtoolbox求解结构静力学问题'
date = 2024-11-02T08:32:48+08:00
draft = false
mathjax = true
categories = ['matlab']
tags = ['matlab', 'tutorial', 'pde', 'structure', 'static']
toc = true
tocBorder = true
+++


## 结构静力学问题

静力学问现在是已经很简单的问题，在材料各向同性的情况下，对于弹性固体材料，很容易通过有限元求解。特别是线弹性问题，方程的矩阵形式可以很容易的写出（准确得说是很容易通过有限元表达），然后通过求解线性方程组得到结果。

基本上，求解的就是如下形式的方程：

$$
m \frac{\partial^2 u}{\partial t^2} + d \frac{\partial u}{ \partial t} - \nabla \cdot (c \nabla u) + a u = f
$$

其中，$m$是质量矩阵，$c$是阻尼矩阵，$a$是刚度矩阵，$f$是外力矩阵, $u$是不同问题中的因变量，结构问题中是位移，热传导问题中是温度，电磁场问题中是电势。而方程中的$d$只有在时变问题中才会出现，是一个时间相关的粘性系数。

当然，当方程的变量有多个时，方程就可以写成矢量的形式：

$$
\mathbf{m} \frac{\partial^2 \mathbf{u}}{\partial t^2} + \mathbf{d} \frac{\partial\mathbf{u}}{\partial t} - \nabla \cdot (\mathbf{c} \otimes \nabla \mathbf{u}) + \mathbf{a} \mathbf{u} = \mathbf{f}
$$

相应的系数就变成矩阵或者向量，式中的 $\otimes$ 也就是张量积或者叫克罗内克积、Kronecker乘积。

接下来我们先介绍下传统方式的求解结构静力学问题，然后介绍新的统一模型的求解方式。

## Matlab PDE Toolbox传统形式

这个工具箱最早的时候就实现了固体结构静力学、热传导、电磁场等问题的求解。




工具箱把不同领域的问题分成了不同的模型，采用同一个接口`createpde`来创建模型。这个接口的第一个参数是模型的名字，根据名称返回不同的模型对象。

- `structural`：结构问题
- `thermal`：热传导问题
- `electromagnetic`：电磁场问题

每种问题，又包含了若干不同的分析类型，就拿结构问题来说，有：

```
"static-solid" | "static-planestress" | "static-planestrain" | "static-axisymmetric" | "transient-solid" | "transient-planestress" | "transient-planestrain" | "transient-axisymmetric" | "modal-solid" | "modal-planestress" | "modal-planestrain" | "modal-axisymmetric" | "frequency-solid" | "frequency-planestress" | "frequency-planestrain" | "frequency-axisymmetric"
```

首先，我们创建一个结构模型。


```matlab
% 创建PDE模型
model = createpde('structural', 'static-solid');

model
```

    
    model = 
    
      StructuralModel - 属性:
    
                  AnalysisType: "static-solid"
                      Geometry: []
            MaterialProperties: []
                     BodyLoads: []
            BoundaryConditions: []
          ReferenceTemperature: []
        SuperelementInterfaces: []
                          Mesh: []
                 SolverOptions: [1x1 pde.PDESolverOptions]
    
    
    

这个`StructuralModel`对象中，有各种属性，可以很容易的猜出其含义。比如`MaterialProperties`就是材料属性，`BodyLoads`就是体力，`BoundaryLoads`就是边界力，`BoundaryConditions`就是边界条件。

下面我们找一个最简单的问题，就是一个悬臂梁的问题，然后求解。


```matlab
% 定义几何
gm = multicuboid(15, 1, 1);
model.Geometry = gm;
% 几何模型
figure
pdegplot(model, 'FaceLabels', 'on', 'FaceAlpha', 0.5);
title('Geometry with Face Labels - Domain Specific Modeling');
```

    
    


    
![png](/matlab-img/structralStatic_output_3_1.png)
    


我们这里专门把面的编号写出来，方便后面的边界条件的设置。

材料特性都是瞎写的。


```matlab
% 指定材料属性
structuralProperties(model, 'YoungsModulus', 210E9, 'PoissonsRatio', 0.3, 'MassDensity', 7800);
```

    
    

接下来就是产生网格，然后求解。


```matlab
% 生成网格
generateMesh(model, 'Hmax', 0.2);
```

    
    

网格的最大边长设为0.1，这个值是可以调整的，这个值越小，网格越密，计算结果越精确，但是计算量也越大。


```matlab
% 绘制网格
figure
pdeplot3D(model);
title('Mesh with Hmax = 0.2');
```

    
    


    
![png](/matlab-img/structralStatic_output_9_1.png)
    


边界条件，我们把梁的右边固定，左边施加一个力。还记得加上重力的影响。看到我们这个力是一个向量，斜向下拉。


```matlab
% 应用边界条件
structuralBC(model, 'Face', 3, 'Constraint', 'fixed');
structuralBoundaryLoad(model, 'Face', 5, 'SurfaceTraction', [0; -1E5; -1E5]);

% 重力
structuralBodyLoad(model, 'GravitationalAcceleration', [0; 0; -9.81]);
```

    
    

调用函数`solve`求解。


```matlab
% 求解PDE
result = solve(model);
```

    
    

首先我们可以绘制整体的位移大小，基本上，就是经典的悬臂梁的感觉。不过，1米的悬臂梁还是很强很强的……


```matlab
% 后处理结果
pdeplot3D(model, 'ColorMapData', result.Displacement.Magnitude);

title('Displacement Magnitude - Static Analysis - Domain Specific Modeling');
```

    
    


    
![png](/matlab-img/structralStatic_output_15_1.png)
    


当然我们还可以计算应力，然后绘制应力的分布，应力集中的方式也很明显。


```matlab
vonMisesStress = result.VonMisesStress;
pdeplot3D(model, 'ColorMapData', vonMisesStress);

title('VonMisesStress-Structural Static-Domain Specific Model');
```

    
    


    
![png](/matlab-img/structralStatic_output_17_1.png)
    


## 统一求解框架

从2023a开始，Matlab开始引入了一个统一的求解框架来替代传统的各个专业的模型。这与工业软件的大的趋势也是一致的，多学科融合、数字化双创等等。

利用统一个数模、或者同一套网格，来求解不同的问题，这样可以减少重复工作，提高效率。

这个框架入口，就是`femodel`。就上面的悬臂梁问题，我们可以用这个框架来求解。


```matlab
model = femodel(AnalysisType = "structuralStatic", Geometry = gm);

model
```

    
    model = 
    
      1x1 femodel array
    
    Properties for analysis type: structuralStatic
    
                AnalysisType: "structuralStatic"
                    Geometry: [1x1 fegeometry]
          MaterialProperties: [0x1 materialProperties]
    
       Boundary Conditions
                      FaceBC: [0x6 faceBC]
                      EdgeBC: [0x12 edgeBC]
                    VertexBC: [0x8 vertexBC]
    
       Loads
                    CellLoad: [0x1 cellLoad]
                    FaceLoad: [0x6 faceLoad]
                    EdgeLoad: [0x12 edgeLoad]
                  VertexLoad: [0x8 vertexLoad]
    
       Other Parameters
        ReferenceTemperature: []
               SolverOptions: [1x1 pde.PDESolverOptions]
    
      Show <a href="matlab:if exist('model','var'), displayAllProperties(model); else, disp('无法显示变量 model 的属性，因为该变量不再存在。'), end">所有属性</a>
    
    
    

这个对象也没有什么特别的，其属性也很容易从名字看出内容。

绘制模型的函数一点都没变……也能够认识这个对象


```matlab
pdegplot(model, FaceLabels='on', FaceAlpha=0.5);
title('Geometry with Face Labels Displayed - femodel');
```

    
    


    
![png](/matlab-img/structralStatic_output_21_1.png)
    


设置材料属性，采用了属性赋值的形式，并提供了一个新的函数`materialProperties`来设置材料属性。

划分网格的函数没有改变，唯一变化的时候必须把模型对象传递出去，不会直接在位修改输入参数的属性，这说明这个对象并不是`handle`对象。

最后，产生的网格记录在了`model.Geometry.Mesh`中，这个对象是一个`FEMesh`对象，这个对象也是一个新的对象，用来表示网格。


```matlab
model.MaterialProperties = materialProperties('YoungsModulus', 210E9, 'PoissonsRatio', 0.3, 'MassDensity', 7800);
model = generateMesh(model);
pdeplot3D(model.Geometry.Mesh);

model.Geometry.Mesh

title('Mesh with default parameters - femodel');
```

    
    ans = 
    
      FEMesh - 属性:
    
                 Nodes: [3x1267 double]
              Elements: [10x600 double]
        MaxElementSize: 0.6027
        MinElementSize: 0.3013
         MeshGradation: 1.5000
        GeometricOrder: 'quadratic'
    
    
    


    
![png](/matlab-img/structralStatic_output_23_1.png)
    


设置边界条件的方式也略有不同，从调用函数修改为了属性赋值的方式。我们可以看到，边界条件变成了一个数组。

- `FaceBC`：面的边界条件，对应`faceBC`对象/函数
- `EdgeBC`：边的边界条件，对应`edgeBC`对象/函数
- `VertexBC`：点的边界条件，对应`vertexBC`对象/函数

载荷条件、初值条件等等也是类似的。

从这三个对象的数组，也可以看到模型包含面、线、点三种对象的数目。当然，设置边界条件的时候，也是按照这个顺序来设置的。

当然，载荷有一个体积载荷，对应`CellLoad`属性和`cellLoad`对象/函数。


```matlab
model.FaceBC(3) = faceBC(Constraint='fixed');

model.FaceLoad(5) = faceLoad(SurfaceTraction=[0; -1E5; -1E5]);

model.CellLoad = cellLoad(Gravity=[0; 0; -9.81]);
```

    
    

当然，我们如果对于Matlab结构数组的操作比较熟悉，也可以对多个对象进行操作，比如设置多个边界条件。

比如面边界，经过我们设置之后，可以看到：


```matlab
model.FaceBC
```

    
    ans = 
    
      1x6 faceBC array
    
    Properties for analysis type: structuralStatic
    
    Index    Constraint    XDisplacement    YDisplacement    ZDisplacement
      1          []             []               []               []      
      2          []             []               []               []      
      3        fixed            []               []               []      
      4          []             []               []               []      
      5          []             []               []               []      
      6          []             []               []               []      
    
      Show <a href="matlab:if exist('ans','var'), displayAllProperties(ans); else, disp('无法显示变量 ans 的属性，因为该变量不再存在。'), end">所有属性</a>
    
    
    


```matlab
model.FaceBC(2:4) = faceBC(Constraint='fixed');

model.FaceBC
```

    
    ans = 
    
      1x6 faceBC array
    
    Properties for analysis type: structuralStatic
    
    Index    Constraint    XDisplacement    YDisplacement    ZDisplacement
      1          []             []               []               []      
      2        fixed            []               []               []      
      3        fixed            []               []               []      
      4        fixed            []               []               []      
      5          []             []               []               []      
      6          []             []               []               []      
    
      Show <a href="matlab:if exist('ans','var'), displayAllProperties(ans); else, disp('无法显示变量 ans 的属性，因为该变量不再存在。'), end">所有属性</a>
    
    
    

这样就很容易的设置了多个边界条件。

接下来就是基本一样的求解过程，调用`solve`函数，然后绘制结果。


```matlab
% 先把面边界条件设回去，不然三个面被固定，结果会很好玩的
model.FaceBC([2, 4]) = faceBC();

ret = solve(model);

pdeplot3D(model.Geometry.Mesh, ColorMapData=ret.Displacement.Magnitude);
title('Displacement Magnitude-Structural Static-femodel');
```

    
    


    
![png](/matlab-img/structralStatic_output_30_1.png)
    



```matlab
vonMisesStress = ret.VonMisesStress;

pdeplot3D(model.Geometry.Mesh, ColorMapData=vonMisesStress);

title('VonMisesStress-Structural Static-femodel');
```

    
    


    
![png](/matlab-img/structralStatic_output_31_1.png)
    


总的来说，统一接口的方式，也并没有什么大的不同之处。我感觉可能会让共享数模来分析不同学科的交叉问题会更加直观一些。当然，后面再学习的时候，建议还是直接从`femodel`开始，因为这个是未来的发展方向。

虽然，资料确实比较少，例子也还有好多没有调整过来。


## 总结

求解静力学问题，求解过程和结果都没有什么让人惊喜的地方。

下面，在讨论一个周期加载的问题，这个问题是一个动力学问题。
