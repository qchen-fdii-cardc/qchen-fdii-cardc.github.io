+++
title = 'Compare_Matrix_Squre_Sum_in_MATLAB中矩阵平方和的比较'
date = 2024-12-04T00:19:52+08:00
draft = false
mathkatex = true
categories = ['matlab']
tags = ['matlab', 'performance', 'matrix', 'tutorial', 'benchmark', 'algorithm']
toc = true
tocBorder = true
+++



## 矩阵平方和的计算

### 矩阵平方和的定义

矩阵平方和的定义是对矩阵中的每一个元素进行平方，然后求和。

对于一个矩阵$A$，其平方和定义为：

$$
\text{sum} = \sum_{i=1}^{m}\sum_{j=1}^{n} A(i,j)^2
$$

这个平方和计算，在某些机器学习的算法中、或者特殊的优化问题中都会涉及到。

### 矩阵平方和的计算方法

通常而言，前面的公式就给出了计算的方法，无论怎么做，都会涉及到对矩阵中的每一个元素进行平方，然后求和。
因此算法的时间复杂度是$O(m*n)$，其中$m$是矩阵的行数，$n$是矩阵的列数，当$A$是方阵时，$m=n$。
最终，算法的效率取决于矩阵的表示形式和对矩阵元素的访问方式。
当然，还有可能会有一些特殊的优化方法，比如矩阵的特殊性质，可以通过一些特殊的方法来计算，这里不做考虑。

最后，就是并行指令集的使用，比如SIMD指令集，这里也不进行相关的讨论。

下面，就主要是对在Matlab中实现矩阵平方和的几种方法进行比较。其实比较的是Matlab中访问矩阵元素的方式的性能差异。

### Matlab的若干实现
我们非常随意的实现了一些方法。

其中最重要的就是所谓的基线方法。这个方法通常选择一个最简单/直观的方法，便于抓住算法中的核心要素。
然后作为对比的基准，也能够对不同算法的性能进行比较。

这种研究办法，是一种常见的研究方法。例如，在优化算法中，通常选择格子搜索或者随机搜索作为基线方法。
提出一种新的方法，通常希望能够对比基线方法有更好的性能，并且这种性能提升是显著的。
此外，也会结合新算法与基线算法的执行过程来解释算法优势的原理，这就是一般算法研究文章中非常重要的理论分析。

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_row_column.m" %}}
```
上面这个最为直观的算法，就是对矩阵的每一行进行遍历，然后对每一行的每一个元素进行平方，然后求和。
这个作为基线是一个恰当的选择。

相应的，我们就会想到先循环列，再循环行来作为对基线算法的改进。
```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_column_row.m" %}}
```

考虑到Matlab能够直接索引列，我们就可以考虑对每列进行循环，直接对列进行操作。或者，换成对行进行操作。
下面几个算法就是行、列操作的形式，这里又有一个小小的差别，就是使用更多内存来直接进行向量加法，最后调用`sum`求和。
或者还可以在循环内部调用`sum`函数，这样内存的使用会从$\mathcal{O}(n)$降低到
```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_column_sum.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_sum_column.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_row_sum.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_sum_row.m" %}}
```

接下来，我们利用矩阵的向量访问方式来构造一个算法。
```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_vec.m" %}}
```

此外，`sum`函数也提供了两个算法变体，一个是调用两次（默认对行求和）
```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_sum_sum.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_sum_all.m" %}}
```

还可以把`sum`与矩阵列向量访问形式结合起来，构成一种算法。
```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_sum_vec.m" %}}
```

最后还是利用矩阵的向量展开方式，就是两个其实一样的算法，因为`dot`函数内部就是调用矩阵乘法。

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_vec_dot.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_matrix_mul.m" %}}
```

这些算法都非常平常，但是，通过上面的联系，也能提升我们对Matlab的矩阵操作的理解。
接下来就是对这些算法的性能进行比较。

## 性能比较
### 性能比较方法
时间性能比较，一般可以直接利用`timeit`函数来完成，这个函数会对一个函数进行多次调用，然后求中位数。
这个函数还能包括输出变量的构造时间。

利用这个函数，我们编了一个工具，对不同规模的矩阵进行比较。
这里代码的注释非常清楚，无需赘述。

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_f_n.m" %}}
```

### 性能比较代码

我们把前面的算法代码和上面的性能比较代码放在一个`+benchobjs`的目录中，就构成一个namespace命名空间，通过`import benchobjs.*`来导入这些函数。
编写如下的测试脚本：

- [Matlab code](/matlab-code/compareMatrixSquareSum.m)
    - 设置测试范围
    - 运行测试获取数据
    - 可视化测试结果

```matlab
{{% codesnap "static/matlab-code/compareMatrixSquareSum.m" %}}
```


### 性能比较结果

最终，可以得到如下的性能比较结果。

![](/matlab-img/compareMatrixSquareSum-time.png)

首先，不同算法的性能有一定差异。基本上，算法分为两组，也可以视为三组。
- 第一组是基线算法，对每一行进行遍历，然后对每一个元素进行平方，然后求和。
- 第二组是向量展开访问并调用矩阵乘法（`mtimes`直接调用BLAS二进制库）的两个算法，性能相当。
- 其他就是各种循环的组合以及`sum`函数的组合。

这个分组，按照加速比来看，更加明显。
向量展开+矩阵乘法的算法在1000~10000的规模下，性能均有显著提升，从15倍到40倍。

![](/matlab-img/compareMatrixSquareSum-acc.png)

除去基线算法和向量化算法，其他算法的关系较为复杂，但是也能通过进行列访问、部分向量化来获得几倍的性能提升。
这充分显示了列优先矩阵访问对与计算效率的影响。

![](/matlab-img/compareMatrixSquareSum-acc-2.png)



## 总结

- 进行算法开发，一定要按照基线算法、算法优化的思路来考虑。
- 对算法的效率进行比较，最好选择不同的规模来分析问题。
- 加速比是一个很好的指标，能够直观的看出算法的性能提升。