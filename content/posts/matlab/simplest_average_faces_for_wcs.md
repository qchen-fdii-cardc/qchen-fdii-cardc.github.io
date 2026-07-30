+++
title = '世界四强平均脸的Matlab实现'
date = 2026-07-30T09:41:31+08:00
draft = false
mathkatex = true
categories = ['matlab', 'statistics', 'aggregation']
tags = ['mean', 'average', 'aggregation', 'fifa', 'average_faces', 'world cup', 'argentina', 'france', 'spanish', 'england']
toc = true
tocBorder = true
+++


<div style="display:flex; gap:8px; flex-wrap:nowrap;">
    <img src="/matlab/average_faces/average_face_england.png" alt="英国" style="width:15%; height:auto;" />
    <img src="/matlab/average_faces/average_face_france.png" alt="法国" style="width:15%; height:auto;" />
    <img src="/matlab/average_faces/average_face_spanish.png" alt="西班牙" style="width:15%; height:auto;" />
    <img src="/matlab/average_faces/average_face_argentina.png" alt="阿根廷" style="width:15%; height:auto;" />
</div>

## 平均

    截止2026年7月28日，马云和我的平均资产达到133亿美元。

### 内涵


平均，是一个非常激进也经常被批评的统计学概念。
平均抹去单个数据中蕴含的丰富信息，无视单个数据的特定故事，
把多个数据作为一个集合来考虑，忽略差异，用单一数据来表达整个集合。
这是一个非常强的概念。

平均，这个动作必须涉及到一个对象集合，这个集合至少应该有2个元素。对于古老的平均概念，特别是原始的平均，这个集合的元素个数必须为正整数，因此这个集合是有限可数的。

$$\{x_1, x_2, \ldots, x_n\}, n - 1 \in \mathbb{Z^+}$$

算术平均数是最常用的平均数，它是集合中所有元素的和除以元素个数。

$$
\bar{x} = \frac{1}{n}\sum_{i=1}^{n}x_i
$$

隐藏一个集合$\{x_1, x_2, \ldots, x_n\}, n - 1 \in \mathbb{Z^+}$，提供单一的聚合量$\bar{x}$，体现了人类思维的抽象能力，忘却差异，归纳总结，排除细节，把个体数据特点置于背景之中，得到一个更有意义的整体信息。

### 算术平均数的应用

在英联邦体系中，长度的定义，foot，英尺。这个单词的原义是脚，也就是一英尺等于一个人的脚的长度。显然，每个人的脚的长度都不一样。那么应该选择英国国王的脚？那么每次更换国王是不是还需要重新丈量已有的土地？还是选择哪个领主的脚？还是选择哪个农民的脚？显然，这中单个个体的选择方法全部都是没有意义的（可以由反证法很容易得到）。并且，每个人的脚还会随着年龄发生变化。16世纪中期的雅各布·科贝尔，给出一个非常实用的定义：在教堂礼拜之后留下16为市民代表，让他们脚挨着脚站在一起，测量整个长度，然后分为16份，得到一个平均的脚长。

![雅各布·科贝尔的英尺](/matlab/average_faces/feet.png)

### 被平均的意义

从最内心的感觉和情感体验上看，平均的激进让很多人都不适应，每个人都觉得自己已经被平均了，自己的独特性、个性、特殊性全部不见了，简直是让人生气，如果不是绝望的话。的确，平均的唯一支撑理念就是：通过忘记、忽略，提供更少的信息，从而得到更加有意义的信息。概括得到的信息超越个体。要接受“更少的信息反而是更多的信息”，需要有很强的心理承受能力。

那么对于个体而言，被平均本身有没有什么意义呢？每一个个体都交叉的属于不同的集合（团体），那么这个团体的各种平均量有没有意义呢？是不是我们也在依赖各种各种的聚合量（包括平均值）来获取心理支撑呢？

1980年，我国城乡居民人均肉类消费为12.70kg；1990年增长到15.91kg；2000年为20.22kg；2011年为28.20kg；2025年，这个数据达到37.9kg[^1]。在我出生的前10年（1980-1990），假设我的智商和知识面神奇地达到后互联网时代的水平，我可能会很容易感觉到自己被平均了，因为我整年都吃不到任何肉食；但是自从2000年之后，我的体重开始随着肉类摄入的整体增长而增长；到2011年，我爱人已经对我的体重增长趋势产生了严重的担忧；现在，我爱人甚至开始限制我的肉类摄入量。

[^1]: [国家统计年鉴](https://www.stats.gov.cn/sj/ndsj/)

总体而言，对个别的样本，平均值会掩盖其个体真实情况；但是平均值的趋势变化，会逐步反应到几乎所有的个体之上。

## Matlab的`mean`函数[^2]

### 函数帮助

我们已经知道了平均的内涵和意义，那么如何在计算机中实现平均呢？在Matlab中，提供了`mean`函数来计算平均值。

```matlab
A = rand(10, 1);
mean(A)
```

这是最简单的平均值计算方法，`mean`函数可以计算向量的平均值，也可以计算矩阵的平均值。甚至`mean`函数还能对三维以上的数组进行平均值计算。


```matlab
M = mean(A)
M = mean(A,"all")
M = mean(A,dim)
M = mean(A,vecdim)
M = mean(___,outtype)
M = mean(___,missingflag)
M = mean(___,Weights=W)
```

这些是`mean`函数的不同用法，大概的含义可以很容易才出来，具体可以参考Matlab文档[^2]。


[^2]: [Matlab文档：mean](https://www.mathworks.com/help/matlab/ref/double.mean.html)


### 加权平均

$$
\bar{x} = \frac{\sum_{i=1}^{n}w_ix_i}{\sum_{i=1}^{n}w_i}
\tag{1}
$$

这里的$w_i$是权重，$x_i$是数据，$n$是数据个数。

实际上，最小二乘法也是一种平均方法，它是通过最小化误差平方和来求解最优参数，从而得到一个最优值，但是也可以从平均的角度来理解最小二乘法。

对于数据集合$\{(x_1, y_1), (x_2, y_2), \ldots, (x_n, y_n)\}$，我们可以通过最小二乘法来拟合一条直线$y = kx + b$，其中$k$是斜率，$b$是截距。

$$
\begin{split}
\hat{k} &=  \frac{\sum_{i=1}^{n} (x_i - \bar{x})(y_i - \bar{y})}{\sum_{i=1}^{n} (x_i - \bar{x})^2} \\
\hat{b} &= \bar{y} - \hat{k}\bar{x} \\
\end{split}
$$

这里的$\hat{k}$可以写为：

$$
\hat{k} =  \frac{
            \sum_{i=1}^{n} (x_i - \bar{x})^2
            \frac{(y_i - \bar{y})}{(x_i - \bar{x})}
        }{
            \sum_{i=1}^{n} (x_i - \bar{x})^2
        }\tag{2}
$$

很容易观察到(2)式与(1)式的类似之处。
实际上，最小二乘法就是对数据点的斜率进行加权平均，权重为$(x_i - \bar{x})^2$，斜率为$\frac{(y_i - \bar{y})}{(x_i - \bar{x})}$。


## 四强

### 平均人

19世纪，均值已经在天文学、测地学等领域取得绝对的支配地位。
但是在社会学中，均值还是一个非常激进的概念，甚至被认为是一个非常不科学的概念。因为社会学中，个体差异非常大，平均值可能会掩盖个体的真实情况。

比利时的统计学家**阿道夫**·凯特勒（Adolphe Quetelet）在19世纪中期提出了“平均人”的概念，他认为通过对大量个体的测量，可以得到一个“平均人”，这个“平均人”可以代表整个群体的特征。这个概念在当时引起了很大的争议，因为它忽略了个体差异，甚至被认为是对个体的不尊重。

实际上，平均人是一个社会中不存在的人，但是，每个种群都有自己的平均人。这个观念被批评和攻击是非常容易理解的……毕竟……你懂的。

到19世纪70年代，**弗朗西斯**·高尔顿（Francis Galton）采用均值的思想来处理和分析非定量数据，通过叠加肖像照片，得到一个“平均脸”，这个“平均脸”可以代表整个群体的特征。这个概念在当时也引起了很大的争议……

下面，我们也来搞一点那啥的研究，得到什么结论你别管……

### 世界四强平均脸

我们在看世界杯的时候就经常讨论，特别是今年挪威队成绩特别好，而蠕动的阿根廷居然进四强简直是逆天……而大热门法国、英国、西班牙也不负众望……其中法国的绝对核心，从以前人见人爱的齐达内到没有存在感的里贝里，再到现在的姆巴佩……英国对虽然有哈里凯恩作为绝对核心，但是队员的构成跟贝克汉姆的时代已经发生了翻天覆地的变化……西班牙的核心现在是亚马尔……

实际上，这个话题还是挺敏感也挺明显的……就是最优秀的黑人运动员逐步也要占领足球的主流地位了……

下面是四强合照……我随便在网上找的，也没有太讲究，不知道是真的还是游戏建模……

![英国](/matlab/average_faces/england.png)
![法国](/matlab/average_faces/france.png)
![西班牙](/matlab/average_faces/spanish.png)
![阿根廷](/matlab/average_faces/argentina.png)

我们就干脆用Matlab来计算一下四强的平均脸吧……


### App[^3]

[^3]: [平均脸Matlab程序源文件](/matlab/average_faces/SimpleFaceAverageApp.m)


这个程序实际还是非常先进的，利用Matlab提供的人脸识别功能，自动识别人脸，并且计算平均脸。

程序的核心算法包括三个部分：

1. 人脸识别，采用vision.CascadeObjectDetector[^4]来完成；
2. 人脸对齐，采用imcrop[^5]和imresize[^6]来完成；
3. 平均脸计算，采用mean[^2]函数来完成。



[^4]: [Matlab文档：vision.CascadeObjectDetector](https://www.mathworks.com/help/vision/ref/vision.cascadeobjectdetector-system-object.html)

[^5]: [Matlab文档：imcrop](https://www.mathworks.com/help/images/ref/imcrop.html)

[^6]: [Matlab文档：imresize](https://www.mathworks.com/help/images/ref/imresize.html)   


用户界面设计非常简单，上方提供按钮来操作导入图像、检测人脸、计算平均、退出程序，下方提供一个图像显示区域来显示导入的图像、检测到的人脸、计算得到的平均脸。

![App 初始界面](/matlab/average_faces/app_init.png) 

程序提供了检测人脸之后，在图像上把人脸按序号标记出来。

![App 运行界面](/matlab/average_faces/app_run.png) 

有些时候，会产生错误的检测结果，程序提供了检查功能，可以查看检测到的人脸，并且可以反选删除错误的人脸，自动重新计算平均脸。

![App 运行界面-检查](/matlab/average_faces/app_check.png) 


程序的代码如下，也可以通过[^3]下载。


```matlab
{{% codeseg "static/matlab/average_faces/SimpleFaceAverageApp.m" %}}
```
