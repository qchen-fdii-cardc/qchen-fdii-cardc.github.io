# Matlab中测量时间

在使用Matalb时，经常需要考虑自己编写代码的效率。在这种情况下，我们需要测量程序运行的时间。Matlab提供了一些函数来测量程序运行的时间。


## 1. 函数简介
本文涵盖的函数包括：

1. tic和toc函数
2. cputime函数
3. timeit函数


### 1.1 tic和toc函数


在Matlab中，我们可以使用tic和toc函数来测量程序运行的时间。tic函数用于开始计时，toc函数用于结束计时并返回时间。

```matlab
tic
% your code
toc
```

### 1.2 cputime函数

cputime函数返回当前进程的CPU时间。CPU时间是指程序在CPU上运行的时间，不包括程序等待I/O的时间。

```matlab
t = cputime;
% your code
t = cputime - t;
```

### 1.3 timeit函数

timeit函数用于测量函数的运行时间。timeit函数会多次运行函数，并返回中位数（median）运行时间来作为最终的评价值。

```matlab
t = timeit(@() your_function());
```





### 1.4 注意事项
这里还需要注意的是，timeit函数与tic/toc函数的运行会相互干扰。因此，不要在同一个函数中同时使用这两种方法。

- 不能在tic和toc之间调用timeit函数。
- 不能用timeit来测试包含tic和toc的函数。
- timeit不能递归调用。

## 2. 高端使用场景

下面用几个例子来对这几个函数主要是`timeit`的用法进行说明。

### 2.1 O(·)时间复杂度的概念

通常我们分析算法的复杂度，会引入O(·)时间复杂度的概念。O(·)时间复杂度是指算法运行时间与输入规模的关系。例如，一个算法的时间复杂度为O(n)，表示算法的运行时间与输入规模n成正比。一个算法的复杂度为O(1)，表示算法的运行时间与输入规模无关。一个算法的复杂度为O(n^2)，表示算法的运行时间与输入规模n的平方成正比。诸如此类，我们可以通过O(·)时间复杂度来表示算法的效率。


那么对我们自己开发的算法，如何测量其时间复杂度呢？这里我们可以使用timeit函数来测量算法的运行时间。

### 2.2 测量算法的运行时间

下面我们通过一个例子来说明如何使用timeit函数来测量算法的运行时间，并绘制算法的运行时间与输入规模的关系。

```matlab
function t = my_algorithm(n)
    % your algorithm
end
```

上面是我们开发的算法的调用方式。下面我们使用timeit函数来测量算法的运行时间。

```matlab
n = 1:1000:10000;
t = zeros(size(n));
for i = 1:length(n)
    t(i) = timeit(@() my_algorithm(n(i)));
end
plot(n, t);
xlabel('Input size');
ylabel('Running time');
```

这个部分写得有点啰嗦，实际上我最近学习了arrayfun之后感觉自己整个人都已经array了，上面的程序可以改得更简单。

```matlab
n = 1:1000:10000;
t = arrayfun(@(x) timeit(@() my_algorithm(x)), n);
plot(n, t);
xlabel('Input size');
ylabel('Running time');
```

绘制曲线之后，还可以通过拟合曲线来得到算法的时间复杂度的估计。

例如，我们测试的算法如果是`rand(n) * rand(n)`，结果可以看到是近似O(n^3)的时间复杂度。这算法的时间复杂度我们可以通过理论分析得到。这里相当是采用实验的方法来验证我们的理论分析。

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/c236c367edf71572d83a667bcd47886f.png#pic_center)





### 2.3 偶然的发现：Matlab的多线程加速

事情在测试`sum((x^2/(x^2+1)).^linspace(0.1, 2.0, x)) / x`的时候，突然有一个很好玩的现象，这里的代码是一个数学公式的计算。

$$
\frac{1}{x} \sum_{i=1}^{x} \left( \frac{x^2}{x^2+1} \right)^{i}
$$

这里，实际上主体的计算是一个`.^`，也就是元素乘方（Element-wise power）。

```matlab
ns=10:500;
times=arrayfun(@(x)timeit(@() sum((x^2/(x^2+1)).^linspace(1, 2.0, x)) / x), ns);
plot(ns, times);
```

当我们测试 $n\in[10, 500]$ 时，一切都很正常，这个算法可以明显看到是O(n)的时间复杂度。

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/6998110c91966f722b778d5b035a9006.png#pic_center)


但是，当我们把范围扩大到 $n\in[10, 1000]$ 时，突然发现了一个很有趣的现象。

```matlab
ns=10:1000;
times=arrayfun(@(x)timeit(@() sum((x^2/(x^2+1)).^linspace(1, 2.0, x)) / x), ns);
plot(ns, times);
```

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/6e4b2b56702ebb294ada32ab7b0fd5e3.png#pic_center)


而且，这个现象非常稳定，n始终出现在511的位置。这个现象是什么呢？经过拍脑袋、看文档，我目前猜测这个现象是Matlab的多线程加速。

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/ba7536f80431393d7765eeb4d0748e90.png#pic_center)


对此，Matlab的文档中有这样的描述：

> Implicit Parallelization with Built-in Multithreading
> 
> Some MATLAB functions implicitly use multithreading to parallelize their execution. These functions automatically execute on multiple computational threads in a single MATLAB session, which means they run faster on multicore-enabled machines. Some examples are linear algebra and numerical functions such as fft, mldivide, eig, svd, and sort. Therefore, if you use these functions on a machine with many cores, you can observe an increase in performance.

> 内置多线程的隐式并行化
>
> 一些MATLAB函数隐式地使用多线程来并行化它们的执行。这些函数在单个MATLAB会话中自动在多个计算线程上执行，这意味着它们在多核启用的机器上运行得更快。一些例子是线性代数和数值函数，如fft、mldivide、eig、svd和sort。因此，如果您在多核机器上使用这些函数，您可以观察到性能的提高。

只是对于矩阵乘法，N=10000还没有表现出多线程加速。

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/8788981cf0b07de274cb68c16d464fb5.png#pic_center)


这个有意思的问题下次再继续研究。

## 3. 总结

本文介绍了Matlab中测量程序运行时间的几种方法。这些方法包括tic和toc函数、cputime函数、timeit函数。这些函数可以用于测量程序运行的时间，用于评估算法的效率。

1. tic和toc函数用于测量程序片段的运行时间。
2. timeit函数用于测量函数的运行时间。
3. timeit可以很方便的进行时间复杂度的分析。
b中测量程序运行时间的几种方法。这些方法包括tic和toc函数、cputime函数、timeit函数。这些函数可以用于测量程序运行的时间，用于评估算法的效率。

1. tic和toc函数用于测量程序片段的运行时间。
2. timeit函数用于测量函数的运行时间。
3. timeit可以很方便的进行时间复杂度的分析。
4. Matlab的隐式多线程加速可能在某些情况下是一个需要考虑的问题。

