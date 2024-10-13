# 在Matlab中使用CoolProp

## 简介

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/55ce2d60e46684b1f2596cc270e47487.png#pic_center)

CoolProp是一个开源的热力学性质库，可以计算多种流体的热力学性质。CoolProp支持多种编程语言，包括Python、C++、Matlab等。本文将介绍如何在Matlab中使用CoolProp。

[CoolProp官网](http://www.coolprop.org/)

本文所使用的Matlab版本为R2021a。

在Matlab中调用CoolProp有两个思路：

1. 利用DLL文件，直接调用CoolProp的C++接口；
2. 利用Python的CoolProp接口，通过Matlab调用Python。

第一种方法需要调用`loadlibrary`函数，然后使用`calllib`函数调用CoolProp的C++接口。这种方法比较复杂，而且需要了解CoolProp的C++接口。

第二种方法则比较简单，只需要调用Python的CoolProp接口即可。

## Matlab的Python接口

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

设置好路径之后，不带参数调用函数`pyenv`可以查看当前Python的路径和执行模式，如下所示：


![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/7171425ed58489c652b47664e7f95ffa.png#pic_center)

## 在Matlab中调用Python

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

那么，接下来就可以在Matlab中调用Python的CoolProp接口了。

## 在Matlab中调用Python的CoolProp接口

在Matlab中调用Python的CoolProp接口，首先需要安装Python的CoolProp库。在Matlab中调用Python的CoolProp接口，需要导入`CoolProp`模块，然后调用`CoolProp.PropsSI`函数即可。

总之是跟在Python中调用CoolProp接口一样的。

下面是一个例子，计算水的饱和水蒸气的焓值：

```matlab
cp = py.importlib.import_module('CoolProp.CoolProp');
h = cp.PropsSI('H','P',101325,'Q',1,'Water');
```

如果只是使用PropsSI函数，我们可以直接导入PropsSI函数，如下所示：

```matlab
PropsSI = py.importlib.import_module('CoolProp.CoolProp').PropsSI;
h = PropsSI('H','P',101325,'Q',1,'Water');
```

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/89e22e9522ad974b082a1b5d47d11d9d.png#pic_center)


另外还可以写一个Matlab函数，来调用`PropsSI`函数，如下所示：

```matlab
function ret = propsSI(varargin)
try
    ret = py.CoolProp.CoolProp.PropsSI(varargin{:});
catch
    error('CoolProp is not installed or not in the system path');
end
end
```

把这个函数放在Matlab的搜索路径下，就可以在Matlab中调用这个函数了。

```matlab
h = propsSI('H','P',101325,'Q',1,'Water');
```

## 总结

1. 在Matlab中调用Python需要先设置Python的路径(`pyenv`)；
2. 在Matlab中调用Python的函数有两种方式，一种是使用`py`函数，一种是使用`py.importlib.import_module`函数；
3. 在Matlab中调用Python的CoolProp接口，需要导入`CoolProp`模块，然后调用`CoolProp.PropsSI`函数即可。
4. 可以写一个Matlab函数，来调用`PropsSI`函数。
使用`py.importlib.import_module`函数；
3. 在Matlab中调用Python的CoolProp接口，需要导入`CoolProp`模块，然后调用`CoolProp.PropsSI`函数即可。
4. 可以写一个Matlab函数，来调用`PropsSI`函数。
5. 在Matlab中调用Python的函数时，需要注意Python的类型。
