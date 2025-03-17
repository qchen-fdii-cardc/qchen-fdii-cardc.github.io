+++
title = 'PDE Plots in GUI应用中使用PDE工具箱的绘图函数'
date = 2025-03-17T11:32:13+08:00
draft = false
mathjax = false
categories = ['matlab', 'FEM']
tags = ['matlab','pdegplot','pdeplot','pdeplot3D','GUI', 'appdesigner']
toc = true
tocBorder = true
+++

## 前言

在网上有一个问题没有得到很好的解决，就是在GUI中显示PDE工具箱的计算结果。目前英文网站上有一个解决方案，非常好玩，先建立一个图窗，把结果绘制在该图窗上，然后用`copyobj`函数把结果复制到GUI的图窗上。还有一个牛人把网格和结果自己插值搞在坐标上。
这两个例子就不要扩散了……但是AI已经被搞坏了，很喜欢这两个方式。

```matlab
figure;
pdeplot(model,'ColorMap','default');
h = get(gcf,'Children');
set(h,'HandleVisibility','off');

% ... uiaxes中绘制

ax = uiaxes(parent);
copyobj(h,ax);
```

还有一个好玩的解放方式，就是把`UIfigure`/`uiaxes`的句柄可见性设置为'on', 然后就能让绘图函数感知到相应的图形对象。

- [STL-file pdegplot in app designer](https://www.mathworks.com/matlabcentral/answers/2119926-stl-file-pdegplot-in-app-designer)
- [Using dendrogram in app designer](https://www.mathworks.com/matlabcentral/answers/705623-using-dendrogram-in-app-designer#answer_587838)


就大概是这样的把戏，但是那个绘图图窗的隐藏老是有这样那样的问题。

我以前也搞了一个更加无聊的产生PDE几何模型的GUI，我的做法是把几何模型导出成stl文件，然后界面上显示STL文件。 用的函数式pdegplot，这个函数本身是支持第一个参数为uiaxes的。

- [PDE几何模型生成器](https://www.windtunnel.cn/posts/matlab/cadqueryeditor/)


而2D结果显示的函数是pdeplot，这个函数也支持在uiaxes中绘制。

```matlab
ax = uiaxes(parent);
pdeplot(ax, model,'ColorMap','default');
```

但是`pdeplot3D`不支持第一个参数`axes`的语法，但是通过`Parent`参数，可以直接在GUI的图窗中绘制。

- [pdeplot3D](https://www.mathworks.com/matlabcentral/answers/1685204-display-pdeplot3d-in-matlab-app)

```matlab
ax = uiaxes(parent);
pdeplot3D(model,'ColorMap','default','Parent',ax);
```

总结一下，PDE工具箱的绘图函数：

| 函数      | 支持的参数                 | 备注 |
| --------- | -------------------------- | ---- |
| pdegplot  | 支持uiaxes作为第一个参数   | 2D   |
| pdeplot   | 支持uiaxes作为第一个参数   | 2D   |
| pdeplot3D | 支持uiaxes作为`Parent`参数 | 3D   |


## 一个简单的GUI

我们来实现一个简单的悬臂梁分析GUI，这个GUI的界面如下：

![APP界面](/matlab/pdeGUI/app.png)

用户可以设置几何参数（长度、宽度、高度），材料参数（杨氏模量、泊松比），载荷参数（力），网格参数（网格尺寸），变形显示参数（变形比例）等参数；并通过按钮开产生几何体、生成网格和求解。结果则在右侧的图窗中显示，包括几何、网格和结果。

### GUI代码 

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 1 57 %}}
```

这里，大部分控件都是不可在外部访问的，`access`属性为`private`。我们把`UIFigure`作为`public`属性，这样就可以在对象中访问，便于调用`exportapp`函数把界面输出图片。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 59 231 %}}
```

实际构造GUI的部分在`createComponents`函数中，这个函数在`simplePDE`函数（构造函数）中调用。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 422 438 %}}
```

### 回调函数

具体的功能在几个回调函数中实现。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 233 264 %}}
```
这个回调函数处理几何体、网格和结果的显示。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 266 348 %}}
```

这个回调函数处理结果的显示，根据一个下拉列表选择的结果类型，显示不同的结果。注意这里`pdeplot3D`的`Parent`参数设置为`app.ResultsAxes`，这样就可以在GUI的图窗中显示结果。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 350 372 %}}
```

这个回调函数产生几何体，就简单调用`multicuboid`函数产生一个长方体，这个函数会把右边的显示切换成几何显示。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 374 386 %}}
```

这个回调函数生成网格，调用`generateMesh`函数生成网格，并把右边的显示切换成网格显示。控制网格尺寸的参数从UI中获取。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 388 412 %}}
```

这个回调函数求解PDE，调用`solve`函数求解PDE，并把右边的显示切换成结果显示。这里边界条件中固定端是`faceBC`，载荷是`faceLoad`。

完成计算后，还可以切换变形量的显示比例。

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" 414 419 %}}
```

大概上面的代码就能够构造一个基本的PDE求解的GUI了。当然，我们还可以增加一些优化过程的GUI控制，构成比较完整的、可以提交给用户的App。

## 完整代码

- [simplePDE.m](/matlab/pdeGUI/simplePDE.m)

```matlab
{{% codeseg "static/matlab/pdeGUI/simplePDE.m" %}}
```
