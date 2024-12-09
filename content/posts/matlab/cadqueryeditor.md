+++
title = 'CAD Editor in Matlab中实现一个CADQuery编辑器'
date = 2024-12-09T08:07:14+08:00
draft = false
mathjax = false
categories = ['matlab', 'FEM']
tags = ['matlab', 'CAD', 'CADQuery', 'FEM', 'Geometry']
toc = true
tocBorder = true
+++


```
陈轸曰：“令尹贵矣，王非置两令尹也。臣窃为公譬可也？楚有祠者，赐其舍人卮酒。
舍人相谓曰：‘数人饮之不足，一人饮之有余。请画地为蛇，先成者饮酒。’
一人蛇先成，引酒且饮之，乃左手持卮，右手画蛇，曰：‘吾能为之足。’
未成，人之蛇成，夺其卮曰：‘蛇固无足，子安能为之足？’遂饮其酒。
为蛇足者，终亡其酒。今君相楚而攻魏，破军杀将得八城，不弱兵，欲攻齐。
齐畏公甚，公以是为名居足矣。官之上非可重也。
战无不胜，而不知止者，身且死，爵且后归，犹为蛇足也。”昭合以为然，解军而去。
                                                 ---西汉·《战国策·齐策二》
```

```
画蛇著足无处用，两鬓雪白趋埃尘。唐·韩愈《感春》
```

## CAD编辑器的构想

反正没有头发，也不怕两鬓雪白趋埃尘，那就画蛇著足吧。

CADQuery是一个Python工具包，用于进行CAD几何建模，是开源工具OpenCascade的一个封装。

我们在Matlab中进行FEM分析，通常会用以下工具来进行建模：

- [预定义几何体](/posts/matlab/structure-static/#matlab-pde-toolbox%E4%BC%A0%E7%BB%9F%E5%BD%A2%E5%BC%8F)中的`multicuboid`函数，还有其他的比如`multicylinder`、`multisphere`等函数
- [CSG](/posts/matlab/2d-geometry-csg/)来描述2D几何
- [几何函数](/posts/matlab/geometry-function/)描述2D几何
- [导入stl](/posts/matlab/fem-meshing/#几何体)

其实，主流的CAD软件系统本身百花齐放，好用的很多，就比如CADQuery，可用性就非常好。本着画蛇添足的精神，我们可以在Matlab中实现一个CADQuery编辑器，用于产生stl模型，并在GUI上显示几何体。

系统功能：

- 输入代码，生成stl模型
- 显示stl模型

大概设计一个GUI就是下面这个子：

![](/matlab-img/cqe-gui.png)

## 实现的技术

### Matlab调用CadQuery
从2014b版本开始，Matlab支持调用Python。在Matlab中调用Python需要先安装Python，然后在Matlab中设置Python的路径。

在Matlab中调用Python之前，应该设置Python的路径，在2019不之前，可以使用`pyversion`函数设置Python的路径，如下所示：

```matlab
pyversion('C:\Python27\python.exe');
```

在2019b版本之后，可以使用`pyenv`函数设置Python的路径，如下所示：

```matlab
pyenv('executable','C:\Python27\python.exe');
```

在R2023b中，则必须调用：

```matlab
pyenv('Version', "C:\ProgramData\miniconda3\python.exe")
```
或者

```matlab
pyenv(Version="C:\ProgramData\miniconda3\python.exe")
```

	请根据自己的版本，`help pyversion`，`help pyenv`，`doc pyversion`，`doc pyenv`灵活处理。

Matlab会自动记住上次设置的`pyenv`。不用每次都设置Python的路径。

但是，每次启动Python（调用Python的函数）之后，就不能更改Python的路径了，必须重启Matlab才能更改Python的路径。

此外，还有一个参数`executionMode`可以设置。`executionMode`有两个值，`inprocess`和`OutOfProcess`，`inprocess`表示在Matlab进程中运行Python，`OutOfProcess`表示在独立的Python进程中运行Python。默认值为`inprocess`。

对于性能关键的应用，建议使用`InProcess`模式。当需要三方库来运行Python代码时（Python库依赖的库与Matlab不兼容），或者需要调试的时候，可以使用`OutOfProcess`模式。

当按照如上的方式设置好Python的路径之后，就可以在Matlab中调用Python了。在Matlab中调用Python的方式有两种：

1. 使用`py`函数；
2. 使用`py.importlib.import_module`函数。



`py`函数可以直接调用Python的函数，`py.importlib.import_module`函数可以导入Python的模块。

使用`py`函数调用Python的函数时，可以直接调用Python的函数，如下所示：

```matlab
py.math.sin(3.14)
 ans =

    0.0016
```

还比如，可以调用Python的`numpy`库，如下所示：

```matlab
py.numpy.array([1,2,3])
```

使用`py.importlib.import_module`函数导入Python的模块，然后调用Python的函数，如下所示：

```matlab
np = py.importlib.import_module('numpy');
np.array([1,2,3])
```

当然这两种方法都是可以的。使用`py`函数调用Python的函数时，需要输入完整的Python函数名，而使用`py.importlib.import_module`函数导入Python的模块，然后调用Python的函数时，只需要输入Python函数名即可。

这里需要注意的是，`python`的常用类型在`matlab`中是不支持的，比如`list`、`dict`、`tuple`等。在`matlab`中，`list`、`dict`、`tuple`等都是`py.list`、`py.dict`、`py.tuple`等类型。哪怕是整数、浮点数等，也是`py.int`、`py.float`等类型。

可以用`class`函数查看`python`的类型，如下所示：

```matlab
class(py.list([1,2,3]))
```

用`detail`函数查看`python`对象的详细信息，包括字段、方法、事件、父类（Matlab中显示为超类）这些。

总的来说，在Matlab中调用Python的函数还是有点憋屈的，不过也能用。

### GUI程序开发
关于这个议题，已经写了很多了，可以参考[GUI标签的所有文章](/tags/gui/)系列的文章。

- [界面开发](/posts/matlab/gui/)
- [界面布局容器](/posts/matlab/ui-layout/)
- [界面设定位置](/posts/matlab/position_layout_ui_in_matlab/)
- [回调函数](/posts/matlab/callbacks/)
- [使用图片](/posts/matlab/uiimage/)

这里唯一需要增加的就是，需要构造一个坐标系来调用`pdegplot`函数，来显示stl模型。

这里，如下代码导入stl模型，并显示：

```matlab
gm = importGeometry('model.stl');
pdegplot(ax, gm, 'FaceAlpha', 0.5, 'FaceLabels', 'on');
```

这里的`ax`是一个坐标系，可以用`uiaxes`函数构造，如下所示：

```matlab
ax = uiaxes(gridLayout);
ax.XLabel.String = 'X';
ax.YLabel.String = 'Y';
ax.ZLabel.String = 'Z';
```

这里注意的是，不是用`xlabel`、`ylabel`、`zlabel`函数，而是用`XLabel`、`YLabel`、`ZLabel`属性的`String`参数来设置坐标标签。


### 工作空间和变量类型

在设计实现上面的这个CADQuery编辑器的时候，考虑到如何把脚本运行的结果（在cadquery中描述为Workplane）保存下来，以便于后续的操作。

最简单的就是把这个变量输出为一个stl文件，然后再导入到Matlab中进行显示。

这里就有一个问题，如何获得脚本中产生的Workplane变量？

最简单的考虑，就是下面这个算法：

1. 找到所有的变量（在哪找？）
2. 看看这个变量是不是Workplane类型（怎么看？）
3. 如果是Workplane类型，就把名字记下来
4. 当需要显示某个Workplane的时候，就把这个Workplane变量导出为stl文件，然后导入到Matlab中显示。

这里就有一个问题，如何找到所有的变量？变量的信息保存在哪里？Matlab如何管理变量？

这里先歪个楼，看看Matlab中函数闭包的问题。

#### 匿名函数闭包

例如，我们定义一个匿名函数，如下所示：

```matlab
f = @(x) x^2+y;
```

如果我们调用这个函数，如下所示：

```matlab
>> f(10)
函数或变量 'y' 无法识别。
出错 @(x)x^2+y 
```

这里就会报错，因为`y`这个变量没有定义。比如，我们再定义这个变量：

```matlab
>> y = 12
y =
    12
>> f(10)
函数或变量 'y' 无法识别。
出错 @(x)x^2+y 
```

依然不行。这里的`y`是一个自由变量，这个变量的值是在函数定义的时候确定的，而不是在函数调用的时候确定的。必须这样：

```matlab
>> y = 12; f = @(x)x^2 + y;
>> f(2)
ans =
    16
```

我们可以通过`functions`函数来查看这个函数的信息，如下所示：

```matlab
>> functions(f)
ans = 
  包含以下字段的 struct:

            function: '@(x)x^2+y'
                type: 'anonymous'
                file: ''
           workspace: {[1×1 struct]}
    within_file_path: ''
```

这里面有一个`workspace`字段，这个字段是一个结构体，这个结构体中保存了这个函数的自由变量的值，如下所示：

```matlab
>> functions(f).workspace{1}
ans = 
  struct with fields:

    y: 12
```

大概说起来，工作空间（workspace）就相当于是一个描述代码运行环境的结构体，这个结构体中保存了所有的变量的值，这些变量的值是在代码运行的时候确定的。

这在其它程序设计语言中，大概就相当于是闭包（closure）。


#### `base`和`caller`工作空间

在Matlab脚本和函数中，就定义了两个特殊的工作空间，`base`和`caller`工作空间。

`base`工作空间是Matlab的基础工作空间，这个工作空间中保存了所有的全局变量的值；`caller`工作空间是调用函数的工作空间，这个工作空间中保存了所有的局部变量的值。

我们可以通过`evalin`函数来查看这两个工作空间的变量，如下所示：

```matlab
>> variables = evalin('base', 'whos')
variables = 
  包含以下字段的 24×1 struct 数组:
    name
    size
    bytes
    class
    global
    sparse
    complex
    nesting
    persistent
```

这会得到一个结构数组，当用`who`代替`whos`时，会得到一个字符串数组，这个字符串数组中保存了所有的变量的名字。这就实际上的提供了一个方法，可以在不同的地方直接操作一个共享的工作空间。


#### 变量类型

Matlab中变量类型可以用`class`函数来查看，如下所示：

```matlab
>> class(1)
ans =
    'double'
>> class('hello')
ans =
    'char'
>> class([1,2,3])
ans =
    'double'
```

通过这个函数与`strcmp`函数，可以判断一个变量是不是某种类型，如下所示：

```matlab
>> strcmp(class(1), 'double')
ans =
  logical
   1
>> strcmp(class(1), 'char')
ans =
  logical
   0
```

## 代码

下面给出实现这个CADQuery编辑器的代码。

### GUI
- [cadqueryEditor.m](/matlab-code/cadqueryEditor.m)

```matlab
{{% codesnap "static/matlab-code/cadqueryEditor.m" %}}
```

### CADQuery代码

这里还有几个实例代码，用于测试CADQuery的功能。

- [mycadquery.cds](/matlab-code/mycadquery.cds)
- [matrixRow2List.m](/matlab-code/matrixRow2List.m)

```matlab
{{% codesnap "static/matlab-code/mycadquery.cds" %}}
```
这里面有一个常用的转换函数，就是把矩阵的行转换为Python的列表。

```matlab
{{% codesnap "static/matlab-code/matrixRow2List.m" %}}
```


## 总结

一个问题：画蛇最好增加几只脚？