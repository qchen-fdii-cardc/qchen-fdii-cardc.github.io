# Matlab中的非数与调试方法

## 是什么？

Matlab编程（计算器使用）中经常有个错误给你，这句话里可能包含一个关键词`NaN`。大部分学生都有过被 `NaN` 支配的痛苦记忆。

`NaN` 是 Not a Number 的缩写，表示不是一个数字。在 Matlab 中，`NaN` 是一个特殊的数值，表示一个无效的或未定义的数值。`NaN` 通常是由于计算错误或者无效的操作导致的。

我们可以通过以下方式创建一个 `NaN`：

```matlab
a = NaN;
```

这个 `NaN` 和Matlab中其他常量，如 `true` 和 `false` 一样，居然是个函数，当我们直接不带括号的引用它时，按照前面的函数调用规则，它相当于就是`NaN()`。

既然是个函数，我们就要看看它的函数签名，即它的输入参数和输出参数。我们可以通过以下方式查看：

```matlab
help NaN
```

    NaN    Not-a-Number.
        NaN is the IEEE arithmetic representation for Not-a-Number.
        A NaN is obtained as a result of mathematically undefined
        operations like 0.0/0.0  and inf-inf.
    
        NaN('double') is the same as NaN with no inputs.
    
        NaN('single') is the single precision representation of NaN.
    
        NaN(N) is an N-by-N matrix of NaNs.
    
        NaN(M,N) or NaN([M,N]) is an M-by-N matrix of NaNs.
    
        NaN(M,N,P,...) or NaN([M,N,P,...]) is an M-by-N-by-P-by-... array of NaNs.
    
        NaN(..., CLASSNAME) is an array of NaNs of class specified by the 
        string CLASSNAME. CLASSNAME can be either 'single' or 'double'.
    
        NaN(..., 'like', Y) is an array of NaNs with the same data type, sparsity,
        and complexity (real or complex) as the single or double precision numeric 
        variable Y.
    
        Note: The size inputs M, N, and P... should be nonnegative integers. 
        Negative integers are treated as 0.
    
        See also inf, isnan, isfinite, isfloat.

    NaN    非数。
        NaN 是 IEEE 算术表示的非数。NaN可以通过数学上未定义的操作获得，例如 0.0/0.0 和 inf-inf。

        NaN('double') 和没有输入的 NaN 是一样的。

        NaN('single') 是 NaN 的单精度表示。

        NaN(N) 是一个 N×N 的 NaN 矩阵。

        NaN(M,N) 或 NaN([M,N]) 是一个 M×N 的 NaN 矩阵。

        NaN(M,N,P,...) 或 NaN([M,N,P,...]) 是一个 M×N×P×... 的 NaN 数组。

        NaN(..., CLASSNAME) 是一个由 CLASSNAME 指定的类的 NaN 数组。CLASSNAME 可以是 'single' 或 'double'。

        NaN(..., 'like', Y) 是一个与单精度或双精度数值变量 Y 相同数据类型、稀疏性和复杂性（实数或复数）的 NaN 数组。

        注意：大小输入 M、N 和 P... 应该是非负整数。负整数被视为 0。

        另见 inf, isnan, isfinite, isfloat.


## 什么样？

`NaN` 是一个特殊的数值，它的特点是：

- 任何数值和 `NaN` 进行运算，结果都是 `NaN`。
  - 向量中包含 `NaN`，那么这个向量的 `sum` 结果是 `NaN`。
- 每一个 `NaN` 都独一无二
  - 两个 `NaN` 之间进行比较，结果也是 `false`
  - `NaN`和任何东西`~=` 比较，结果都是 `true`。 
  - 任何数值和 `NaN` 进行大小（不等于除外）比较，结果都是 `false`。
- 可以通过函数 `isnan` （R2006a）或者 `ismissing` （R2013b）来判断一个数值是否是 `NaN`。
- 可以通过函数 `anynan` (R2022a) 或者 `anymissing` (R2022a) 来判断一个数组中是否包含 `NaN`。
- `rmmissing` (R2016a)可以用于检测和删除数组中的 `NaN`。
- `fillmissing` (R2016b)可以用于填充数组中的 `NaN`。

上面这些函数，在不同的版本中引入，使用时要注意版本兼容性。具体的调用可以通过 `help` 函数查看，或者采用 `doc` 函数查看详细的文档。

## 怎么办？

在编程中，我们应该尽量避免产生 `NaN`。如果我们的程序中出现了 `NaN`，那么我们应该检查程序的逻辑，找出产生 `NaN` 的原因，进一步修复它们或者利用 `NaN` 作判断。

Matlab提供非常好的调试工具。大概我们调试Matlab程序有两种方式：

1. 通过对怀疑的变量进行 `isnan` 判断，找出产生 `NaN` 的原因，以前我都是这么做的。
2. 通过 `dbstop if naninf` 命令，可以在程序中出现 `NaN` 或者 `Inf` 时，自动停止程序，方便我们调试。

这个函数`dbstop`在R2016a之后就有了。当然，有了坦克和大炮，谁还会用长矛呢。


假设我们有个脚本或者函数，

```matlab
a = zeros(1, 20);

b = a ./ a;
```

在命令行中，我们输入

```matlab
dbstop if naninf
```

运行脚本，用编辑器的运行按钮，或者在命令行中输入函数或者脚本名称，Matlab的运行自然会停留在产生`NaN`的地方。

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/d4ca9d07501003f79e108907fa63b5a4.png)


这个时候我们就可以逐一检查算式中的变量，找出产生`NaN`的原因。

## 结论

1. `NaN` 是 Matlab 中的一个特殊数值，表示不是一个数字。
2. `NaN` 有一些特殊的性质，我们可以通过函数来判断和处理。
3. 文中提到的函数，有的是在较新的版本中引入的，使用时要注意版本兼容性。










