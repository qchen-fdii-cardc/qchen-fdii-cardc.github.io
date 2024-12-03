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

### Matlab的若干实现

#### 循环实现

```matlab
function sumRet = matrix_square_sum(A)
    [m, n] = size(A);
    sumRet = 0;
    for i = 1:m
        for j = 1:n
            sumRet = sum + A(i,j)^2;
        end
    end
end
```


#### 矩阵乘法实现

```matlab
function sumRet = matrix_square_sum(A)
    sumRet = sum(sum(A.^2));
end
```

#### 矩阵元素平方和实现

```matlab
function sumRet = matrix_square_sum(A)
    sumRet = sum(A(:).^2);
end
```






## 性能比较
### 性能比较结果

![](/matlab-img/compareMatrixSquareSum-time.png)

![](/matlab-img/compareMatrixSquareSum-acc.png)

![](/matlab-img/compareMatrixSquareSum-acc-2.png)


### 性能比较代码


```matlab
{{% codesnap "static/matlab-code/compareMatrixSquareSum.m" %}}
```

