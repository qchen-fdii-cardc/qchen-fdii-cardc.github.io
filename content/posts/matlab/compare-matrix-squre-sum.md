+++
title = 'Compare_Matrix_Squre_Sum_in_MATLAB中矩阵平方和的比较'
date = 2024-12-04T00:19:52+08:00
draft = true
mathjax = true
categories = ['matlab']
tags = ['matlab', 'performance', 'matrix', 'tutorial']
toc = true
tocBorder = true
+++



## 矩阵平方和的计算

### 矩阵平方和的定义

矩阵平方和的定义是对矩阵中的每一个元素进行平方，然后求和。

对于一个矩阵 $A$ ，其平方和定义为：

$$
\text{sum} = \sum_{i=1}^{m}\sum_{j=1}^{n} A(i,j)^2
$$

### 算法

这个问题的算法非常简单，但是也可能是研究算法的一个很好的示例，因为简单所以没有理解障碍，更好地展示进行算法研究的步骤和精细之处。

### 算法基线

通常，在进行算法研究时，我们会首先确定一个基线算法，然后对算法进行逐步改善，有些时候这些改善是针对特定的问题，有些时候是针对特定的硬件/软件平台。

我们就以这个行、列循环求和的算法作为基线算法。这个算法非常自然和直观，也没有任何引起误解的地方，其性能也很容易分析。

```matlab
function sumRet = bench_loop_row_column(A)
    [m, n] = size(A);
    sumRet = 0;
    % 对每行进行循环
    for i = 1:m
        % 对每列进行循环
        for j = 1:n
            sumRet = sumRet + A(i, j) ^ 2;
        end
        % 列计算结束
    end
    % 行计算结束
end
```

当，$m==n$时，这个算法的时间复杂度是 $O(n^2)$ ，因为基本的平方和相加操作需要进行 $n^2$ 次。

### 算法改进
算法基线除了考虑时间复杂度之外，还会考虑空间复杂度、结果正确性。

- 结果正确性：对于优化算法，可能是逼近结果的效率
- 空间复杂度：内存占用
- 时间复杂度：运行时间，或者说运行时间对问题规模的依赖关系

对于这个确定性算法，结果正确性我们暂时可以放在一边，要求肯定是必须计算正确（！），其实这个问题没那么简单，涉及到浮点数的表达和计算，但是我们暂时不考虑这个问题。


## Matlab的若干实现

#### 循环实现





#### 矩阵乘法实现


#### 矩阵元素平方和实现







## 性能比较
### 性能比较结果

![](/matlab-img/compareMatrixSquareSum-time.png)

![](/matlab-img/compareMatrixSquareSum-acc.png)

![](/matlab-img/compareMatrixSquareSum-acc-2.png)


### 性能比较代码


```matlab
{{% codesnap "static/matlab-code/compareMatrixSquareSum.m" %}}
```

