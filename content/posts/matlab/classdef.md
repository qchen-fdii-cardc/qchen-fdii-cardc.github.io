+++
title = 'MATLAB中的类定义基础知识'
date = 2025-03-08T20:52:55+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'classdef', 'OOP', 'object-oriented']
toc = true
tocBorder = true
+++


## Matlab的类

为什么需要面向对象？

在Matlab的官方文档中是这么解释的：

```
采用创建类的方式，可以简化某些类型的编程任务，
这类任务包括涉及到特定的数据结构、
或者跟特定数据联系在一起的大量函数。
```

这里我们可以看到，实际上还是数据与操作数据的函数的封装。把非常靠近的数据机构与操作绑定在一起，便于管理。

Matlab的类支持函数、操作符的重载，支持属性和方法的访问控制，支持引用类型和值类型，并且支持事件和回调（listener）。

## 类定义

### 类定义的语法

从2008a开始，Matlab支持用关键词`classdef`定义类。类定义的语法如下：

```
classdef (Attributes) ClassName < SuperclassNames
    properties (Attributes) ... end
    methods (Attributes) ... end
    events (Attributes) ... end
    enumeration ... end
end
```

此处，`ClassName`是类名，`SuperclassNames`是父类名。`properties`、`methods`、`events`、`enumeration`    是类定义的组成部分。同样，`properties`、`methods`、`events`、`enumeration`也时用于查询类信息的函数。

在`classdef`、`properties`、`methods`、`events`、`enumeration`后面可以跟属性设置，这些属性设置可以控制类定义的各个部分。这个属性设置的语法如下：

```
Attributes ::= PropertyName1=PropertyValue1, PropertyName2=PropertyValue2, ...
```

而Matlab的类继承的语法如下：

```
SuperclassNames ::= SuperclassName1 & SuperclassName2& ...
```

这样可以实现多个父类的继承。

当我们把一个类的所有代码都放在一个文件中时，这个文件只需要包含在Matlab的搜索路径中即可。当我们把类的部分代码定义在多个不同文件中时，对于目录和文件的组织方式有一定的要求。

### 属性

属性是类中数据结构和操作的封装。在属性字段中，可以设置属性的访问权限，完整的属性包括：

| 属性                   | 含义                 |
| ---------------------- | -------------------- |
| `AbortSet`             | 是否在设置属性时中止 |
| `Abstract`             | 是否为抽象属性       |
| `Access`               | 访问权限             |
| `Constant`             | 是否为常量属性       |
| `Dependent`            | 是否为依赖属性       |
| `GetAccess`            | 获取权限             |
| `GetObservable`        | 是否可观测           |
| `Hidden`               | 是否隐藏             |
| `NonCopyable`          | 是否不可复制         |
| `PartialMatchPriority` | 部分匹配优先级       |
| `SetAccess`            | 设置权限             |
| `SetObservable`        | 是否可设置           |
| `Transient`            | 是否为瞬态属性       |


详细的设置只有参考文档中的`Property Attributes`章节。

属性还支持完整的尺寸和类型检查以及默认值，其设定语法与`arguments`的语法类似。

```
properties (attributes)
   propName1 (dimensions) class {validators} = defaultValue
   ...
end
```

例如：

```
properties
    PropertyName1 (1,1) double {MustBePositive} = 1.0
    PropertyName2 (1,1) double = [1, 2, 3]
end
```

对属性的访问可以通过`obj.PropertyName`的方式进行。当然，还支持把属性名存储为一个字符串或者字符数组，然后通过`obj.(PropertyName)`的方式进行访问。

### 方法

#### 方法定义

方法定义了类中可以执行的函数。方法的定义语法如下：

```
methods (attributes)
   function method1(obj,arg1,...)
      ...
   end
   function method2(obj,arg1,...)
      ...
   end
   ...
end
```

这里面的方法定义（包括输入参数、输出参数、函数体），跟普通的函数定义式完全一样的。只是函数的第一个参数，默认为传递类对象。具体函数定义语法、参数限定、输出参数等等，都可以参考函数定义的部分。

#### 在不同文件中定义类方法

当然，Matlab还提供了一种通过类定义文件夹的方式来组织类的方法。
在这种方式下，类定义文件夹中必须包含一个`@ClassName`的子文件夹，其中`ClassName`是类名。在`@ClassName`文件夹中，必须包含一个`ClassName.m`文件，这个文件中定义了类的申明部分。类的方法，可以定义在`ClassName.m`文件中，也可以定义在其他文件中。

整个构成的文件结构如下：

```
ClassName
   @ClassName
      ClassName.m
      method1.m
      method2.m
```

一般而言，推荐在`ClassName.m`文件中同时申明方法，并限定方法的访问权限等属性，方法的定义则可以放在对应的`method1.m`、`method2.m`等文件中。

这个时候，在类定义文件中，方法的定义语法如下：

```
methods (attributes)
    output = method1(obj,arg1,...)
   ...
end
```

而对应的方法定义文件则是一个普通的函数定义文件，其语法如下：

```
function output = method1(obj,arg1,...)
    ...
end
```

#### 类的构造

类的构造函数是用于创建类对象的函数。构造函数的定义语法如下：

```
function obj = ClassName(arg1,arg2,...)
    ...
end
```
这个函数同样是一个普通函数，只是其返回值是类对象，参数中不需要传递类对象。

### 事件

事件是类中的一种特殊的方法，用于在类中触发事件。事件及其处理函数包括以下部分：

1. 事件名称，在对应处理的`handle`类中申明。
2. 事件触发函数，当特定的条件满足时，触发事件。
3. Listener对象，用于监听触发事件。
4. 事件数据，用于传递事件相关的数据。

#### 命名事件

命名事件的定义语法如下：

```
events (attributes)
    EventName1
end
```

#### 触发事件

触发事件的函数语法如下：

```
notify(obj,EventName1)
```

#### 监听事件

监听事件的函数语法如下：

```
listener = addlistener(obj,EventName1, callback)
```

这里，`callback`是一个函数句柄，用于处理事件，例如可以用`@`符号来指定一个函数句柄。

这个函数返回一个`listener`对象的句柄，如果我们要取消监听，则可以调用`delete`函数；如果要临时取消监听，可以利用`listener.Enabled`属性。

#### 事件数据

事件数据是用于传递事件相关的数据。事件数据类继承自`event.EventData`类，并可以重载`event.EventData`类中的方法。当我们需要传递事件相关的数据时，可以创建一个事件数据类的对象，并将其作为参数传递给`notify`函数。

```matlab
classdef MyEventData < event.EventData
    properties
        Data1
        Data2
    end
    methods
        function obj = MyEventData(d1, d2)
            obj.Data1 = d1;
            obj.Data2 = d2;
        end
    end
end
```

这样在触发事件时，可以传递事件数据：

```matlab
notify(obj,EventName1,MyEventData(d1, d2))
```

### 枚举

枚举是类中的一种特殊的数据结构，用于定义一组常量。枚举的定义语法如下：

```
enumeration (attributes)
    EnumName1
    EnumName2
    ...
end
```

举个例子，我们要定义一个表示星期的枚举：        


```matlab
classdef Weekday
    enumeration
        Monday,Tuesday,Wednesday,Thursday,Friday
    end
end
```
当然，枚举类也不过是一个普通的类，所以可以继承自其他类，也可以重载其他类的方法。

如果用枚举来配合`properties`，则可以实现枚举类型的属性。

```matlab
classdef Weekday
    properties
        Day
    end
    methods
        function obj = Weekday(i)
            arguments
                i (1,1) double {mustBeInteger, mustBePositive, mustBeMember(i,1:5)} = 1
            end
            obj.Day = i;
        end
    end
    enumeration
        Monday(1),Tuesday(2),Wednesday(3),Thursday(4),Friday(5)
    end
end
```

这样，我们就可以通过`Weekday.Day`来访问枚举类型的值。

## 总结

Matlab的类支持方式，相对来说比较直观，也比较容易理解。
当然，这里还没有介绍到类的一些高级特性，例如：

- 类的继承
- 类的方法、属性的重载
- handle类和value类

一般而言，面向对象的诸多特性，在Matlab中都有对应的实现。

