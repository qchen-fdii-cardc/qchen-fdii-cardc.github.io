# Matlab中的控制流

虽然，我们说Matlab中的计算是向量化的，但是在某些情况下，作为一个“程序设计语言”，Matlab也提供了一些控制流结构，来帮助我们实现一些复杂的逻辑。

我会在介绍控制流的时候，提醒如何用向量化的方式来实现相同的功能。

首先是一个总结的表格：

| 控制流                  | 说明                                                     |
| ----------------------- | -------------------------------------------------------- |
| if, elseif, else        | 根据条件是否为`true`执行相应语句                         |
| switch, case, otherwise | 从一组语句中选择性执行                                   |
| for                     | 循环执行一定的次数                                       |
| while                   | 当条件为真时一直执行                                     |
| try, catch              | 执行语句并捕捉异常                                       |
| break                   | 中断`for`和`while`循环                                   |
| return                  | 中断脚本和函数的执行                                     |
| continue                | 跳过`for`和`while`循环的剩下部分，并进入下一个循环的执行 |
| pause                   | 暂时停止Matlab的运行                                     |
| parfor                  | 并行版本的`for`循环                                      |
| end                     | 终止代码块，或者表示最后一个索引                         |

## 1. 条件语句

Matlab中的条件语句有`if`、`else`、`elseif`和`end`。

```matlab
if condition1
    % do something
elseif condition2
    % do something
else
    % do something
end
```

这是一个完整的条件语句的结构，`elseif`和`else`是可选的。按照程序设计中尽快返回的原则，我们总是会把能够马上结束逻辑的条件放在前面。

此外还有一个多分支的条件语句`switch`。

```matlab
switch expression
    case condition1
        % do something
    case condition2
        % do something
    otherwise
        % do something
end
```

`switch`语句中，`case`和`otherwise`是可选的。`switch`语句的执行逻辑是，从上到下，遇到第一个满足条件的`case`，就执行对应的逻辑，然后跳出`switch`语句。如果没有满足条件的`case`，就执行`otherwise`的逻辑。

对于数字，测试case_expression == switch_expression。
对于字符向量，测试strcmp(case_expression,switch_expression) == 1。
对于支持`eq`函数的对象，case_expression == switch_expression。重载的eq函数的输出必须是逻辑值或者可以转换为逻辑值。
对于一个元胞数组case_expression，至少有一个元素与switch_expression匹配，这里的元素可以是数字、字符向量和对象。


## 2. 循环语句

Matlab有几种方式实现循环，`for`、`while`。

### 2.1 `for`循环

`for`循环的结构如下：

```matlab
for i = 1:10
    % do something
end
```

这是一个从1到10的循环，`i`是循环变量，可以在循环体内使用。

### 2.2 `while`循环

`while`循环的结构如下：

```matlab
i = 1;
while i <= 10
    % do something
    i = i + 1;
end
```

在循环中，可以采用`break`和`continue`来控制循环的执行。这跟其它的编程语言是一样的。




## 3. 向量化循环条件语句

在Matlab中，我们可以用逻辑索引来实现循环+条件。我们很随意地指定一个例子，就是随机N个数，然后统计大于0.5的数的个数。这个结果不用看，肯定会在N趋向于无穷大时收敛到0.5*N。

第一个版本，采用`for`循环和`if`条件语句：

```matlab
function count = forIf(N)

a = rand(1,N);
count = 0;
for ai = a
    if ai > 0.5
        count = count + 1;
    end
end
```

第二个版本，只有一行代码，采用逻辑索引和`sum`函数：

```matlab
function count = logicalIndex(N)
a = rand(1,N);
count = sum(a>0.5);
```

这里的内存复杂度很容易看出来，`forIf`的内存复杂度是O(1)，而`logicalIndex`的内存复杂度是O(N)。后者产生了一个零时的逻辑索引数组，然后再计算逻辑索引的和。

下面来看看时间复杂度。

```matlab
t1 = @(n) timeit(@() forIf(n));
t2 = @(n) timeit(@() logicalIndex(n));

n = round(logspace(3, 8, 10));

t1s = arrayfun(t1, n);
t2s = arrayfun(t2, n);

plot(n, t1s, 'r', n, t2s, 'g');

legend({'For loop', 'Logical Index'});
xlabel('n');
ylabel('Time with timeit');
print -dpng -r600 compare

figure
semilogx(n, t1s ./ t2s, 'h', 'linewidth', 2)
xlabel('n');
ylabel('Acceleration');
print -dpng -r600 ar
```


![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/f3da3e43cdb9a02c5058c4560ed49837.png#pic_center)



很容易看出，两个算法的时间复杂都是O(N)，但是`logicalIndex`的速度要快很多，具体来说就是快2到3倍。

![在这里插入图片描述](https://i-blog.csdnimg.cn/blog_migrate/bf4ed746dff4afdb0097e20eac3a65b7.png#pic_center)


## 4. 结论

大概来看，Matlab中的控制流结构和其它编程语言是一样的。

但是在Matlab中，我们可以用向量化的方式来实现循环和条件语句，这样可以提高代码的可读性和运行速度。

可以看到，要获得更好的时间性能，必然会牺牲空间性能，这是一个典型的时间空间权衡问题。对于某些要按照时间步长计算的问题，要得到更快的计算速度，我们也会把所有时间不长的数组空间都事先申请，并写成矩阵计算的方式来实现，从而提高计算速度。
