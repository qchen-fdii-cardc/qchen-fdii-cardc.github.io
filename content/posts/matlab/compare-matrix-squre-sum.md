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


```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_row_column.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_column_row.m" %}}
```

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

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_loop_vec.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_sum_sum.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_sum_all.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_sum_vec.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_vec_dot.m" %}}
```

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_matrix_mul.m" %}}
```



## 性能比较
### 性能比较方法

```matlab
{{% codesnap "/static/matlab-code/+benchobjs/bench_f_n.m" %}}
```


### 性能比较结果

![](/matlab-img/compareMatrixSquareSum-time.png)

![](/matlab-img/compareMatrixSquareSum-acc.png)

![](/matlab-img/compareMatrixSquareSum-acc-2.png)


### 性能比较代码

- [Matlab code](/matlab-code/compareMatrixSquareSum.m)

```matlab
{{% codesnap "static/matlab-code/compareMatrixSquareSum.m" %}}
```

