+++
title = 'Arguments_in_Matlab中的函数参数'
date = 2024-10-29T09:28:14+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'arguments', 'function']
toc = true
tocBorder = true
+++


## 函数参数

当我们定义函数时，我们可以使用如下的语法形式。当然Matlab中，并没有参数类型标识的语法，所以这里的x1,...,xM只是一个形式，实际上可以是任何类型的参数。

```matlab
function [y1,...,yN] = myfun(x1,...,xM)
```

古早时期，我学Matlab的时候，通常会在函数中调用如下几个函数来进行处理，判断函数的类型、验证函数参数的有效性、实现函数的多态性。

- nargin
- nargout
- varargin
  

在2019b版本之后，Matlab引入了新的函数参数处理语法`arguments...end`，这个方式更加直观，也更加方便。

```matlab
function [y1,...,yN] = myfun(x1,...,xM)
    arguments
        x1 (1,1) double
        x2 (1,1) double
        ...
        xM (1,1) double
    end

    % 函数体
end
```

这个语法已经有四种形式，分别是：

- 输入参数
- 重复输入参数
- 输出参数
- 重复输出参数

## 输入参数

对于输入参数，我们可以使用如下的语法形式。

```matlab
arguments
    argName1 (dimensions) class {validators} = defaultValue
    ...
    argNameN ...
end
```

这里，argName是参数的名字，dimensions是参数的维度，class是参数的类型，validators是参数的验证函数，defaultValue是参数的默认值。

- argName：形式参数的名字，可以是任意合法的变量名，必须出现在函数的定义中。
- dimensions：形式参数的维度，至少要一个维度，`(1,1)`表示标量，`(1,:)`表示行向量，`(:,1)`表示列向量，`(:,:)`表示矩阵，只能是常量。
- class：形式参数的类型，可以是任意合法的类型，如`double`、`char`、`string`、`logical`、`cell`、`struct`、`table`、`function_handle`等。
- validators：形式参数的验证函数集合（元胞数组），函数名称，用于验证参数的有效性，这个函数会在参数不恰当时用`throwAsCaller`来抛出错误。
- defaultValue：形式参数的默认值，如果没有提供参数，那么就会使用这个默认值，这样就能实现类似于Python中的默认参数的功能。

这里的几个要素，都是可选的，可以根据实际情况来选择是否使用。

```matlab
function ret = abc(x1, x2, x3)
    arguments
        x1 (1,1) double  = - 1.0
        x2 (1,1) double = 0.0
        x3 (1,1) double = 1.0
    end
    ret = x1 + x2 + x3;
end
```

调用的方式可以是：


```matlab
[abc(), abc(1), abc(1,2), abc(1,2,3)]
```

    
    ans =
    
         0     2     4     6
    
    
    

当我们调用函数`abc`时，如果输入了不符合的参数，那么就会抛出错误。

### 验证函数

Matlab提供了一些内置的验证函数，可以用于验证参数的有效性，常见的，有如下几种。

- 数值特性
  - 大于0，`mustBePositive`
  - 大于等于0，`mustBeNonnegative`
  - 小于0，`mustBeNegative`
  - 小于等于0，`mustBeNonpositive`
  - 整数，`mustBeInteger`
  - 有限数，`mustBeFinite`
- 比较特性
  - 大于某个值，`mustBeGreaterThan`
  - 小于某个值，`mustBeLessThan`
- 数据类型
  - 数值，`mustBeNumeric`
  - 必须是某个类型，`mustBeA`
- 大小
  - 非空，`mustBeNonempty`
  - 行向量，`mustBeRow`
  - 列向量，`mustBeColumn`
  - 矩阵，`mustBeMatrix`
- 范围和成员
  - 大小在某个范围内，`mustBeInRange`
  - 是某个集合的成员，`mustBeMember`
- 文本
  - 文件，`mustBeFile`
  - 文件夹，`mustBeFolder`
- 类型
  - 类型，`mustBeType`
  - 类型或者空，`mustBeTypeOrEmpty`
  - 有效变量名称，`mustBeValidVariableName`


如果内置的验证函数不能满足需求，我们也可以自定义验证函数，只需要满足如下的条件。

- 函数的输入参数是要验证的参数。
- 函数内部使用`throwAsCaller`来抛出错误。
- 或者调用`error`来抛出错误。

在使用自定义验证函数时，我们可以使用如下的语法形式。

```matlab
function ret = nonsense(x1, x2, x3)
    arguments
        x1 {myValidator1}
        x2 {myValidator2(x2, 10)}
        x3 {myValidator3(x3, x1)}
    end
    ret = numel(x1) + numel(x2) + numel(x3);
end

function myValidator1(x)
    if ~isreal(x)
        throwAsCaller(MException('myValidator1:NotReal', 'The input must be real.'));
    end
end

function myValidator2(x, y)
    if numel(x) < y
        throwAsCaller(MException('myValidator2:SizeTooSmall', sprintf('The input  size must be greater than %d.', y)));
    end
end

function myValidator3(x, y)
    if numel(x) < numel(y)
        throwAsCaller(MException('myValidator3:X2SizeTooSmall', 'The input size x3 must not less than size x1.'));
    end
end
```

可以看到，这个函数有几个特点：当函数的参数就是形式参数时，可以忽略参数名称，直接写`{myValidator1}`；当函数的参数除形式参数外还有别的参数时，需要写成完整的函数调用形式，
`{myValidator2(x2, 10)}`，在调用中，可以使用已经定义的形式参数，也可以使用常量。

这个`throwAsCaller`函数，可以用于抛出错误，这个错误会在调用函数的地方抛出，而不是在验证函数的地方抛出，这样就能更好的定位错误。


```matlab
nonsense(1,1:11,3)
```

    
    ans =
    
        13
    
    
    

### 命名参数的使用

在`arguments`块中，用类似于结构体的方式来定义参数，这样就可以实现命名参数的功能。

```matlab
function printScore(x, options)
    arguments
        x (1,1) double = 0
        options.Name (1,1) string = "Anonymous"
        options.Age (1,1) double = 18
    end

fprintf('name: %s, age: %d, score: %f\n', options.Name, options.Age, x);
end
```

这样，我们就可以使用如下的方式来调用函数。


```matlab
printScore()
printScore(60)
printScore(60, Name="John Doe")
printScore(60, Age=12)
printScore(60, Name="John Doe", Age=12)
```

    name: Anonymous, age: 18, score: 0.000000
    name: Anonymous, age: 18, score: 60.000000
    name: John Doe, age: 18, score: 60.000000
    name: Anonymous, age: 12, score: 60.000000
    name: John Doe, age: 12, score: 60.000000
    
    

## 输出参数

对于输出参数，我们可以使用如下的语法形式。除默认值之外，与输入参数的定义方式是一样的。

```matlab
arguments(Output)
    argName1 (dimensions) class {validators}
    ...
    argNameN ...
end
```

举个例子：

```matlab
function [xfinal,yfinal] = rotatePatch(angle)
    arguments (Output)
        xfinal {mustBePositive}
        yfinal {mustBePositive}
    end
    xfinal = cos(angle);
    yfinal = sin(angle);
end
```


```matlab
% 获得输出的个数
n = nargout(@printScore);
% 利用逗号分割列表赋值输出的方式调用函数
[xy{1:n}] = rotatePatch(0.1 * pi);
% 利用逗号分割列表来把输出转化为数组
[xy{:}]
```

    
    ans =
    
        0.9511    0.3090
    
    
    

## 重复参数

### 重复输入参数
当我们需要类似于`plot`函数的重复参数时，可以使用如下的语法形式。

```matlab
arguments (Repeating)
    argName1 (dimensions) class {validators}
    ...
    argNameN
    ...
    argNameM (dimensions) class {validators}
end
```

这是，我们就能采取如下的方式来调用函数。

```matlab
func(arg1_1, ..., arg1_M, arg2_1, ..., arg2_M, arg3_1, ..., arg3_M)
```

而在函数的定义中，形式参数就会时一个元胞数组，这个元胞数组就会包含所有的重复参数。

```matlab
argName1 = {arg1_1, ..., arg3_1}
argName2 = {arg1_2, ..., arg3_2}
...
argNameM = {arg1_M, ..., arg3_M}
```

例如，我们可以定义一个函数，用于绘制多条曲线：

```matlab
function fRepeat(x,y,style)
    arguments (Repeating)
        x (1,:) double
        y (1,:) double
        style {mustBeMember(style,["--",":"])}       
    end
    
    % Reshape the cell arrays of inputs and call plot function
    z = reshape([x;y;style],1,[]);
    if ~isempty(z)
        plot(z{:});
    end
end
```

x1 = 1:10;
y1 = 1:10; 
s1 = ":"; 
x2 = 1:7;
y2 = 1:1.5:10;
s2 = "--";
fRepeat(x1,y1,s1,x2,y2,s2)

### 重复输出参数

同样，也可以设置重复输出参数：

```matlab
function vectorSum = repeatSum(a,b)
  arguments (Input,Repeating)
    a (1,:)
    b (1,:)
  end
  arguments (Output,Repeating)
    vectorSum (1,:)
  end

  n = numel(a);
  vectorSum{n} = a{n} + b{n};
  for i = 1:n-1
    vectorSum{i} = a{i} + b{i};
  end
end
```


```matlab
x1 = [1 2];
y1 = [3 4];
x2 = [1; 0];
y2 = [0; 1];
[sum1,sum2] = repeatSum(x1,y1,x2,y2)
```

    
    sum1 =
    
         4     6
    
    
    sum2 =
    
         1     1
    
    
    

## 总结

1. `arguments`块中，可以定义输入参数、输出参数、重复输入参数、重复输出参数。
2. 可以使用内置的验证函数，也可以自定义验证函数。
3. 可以使用结构体的方式来定义命名参数。
4. `throwAsCaller`可以用于抛出错误, `error`也可以用于抛出错误。
