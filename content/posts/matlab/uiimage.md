+++
title = 'UIImage_in_Matlab图形界面开发中的图片'
date = 2024-10-26T09:21:18+08:00
draft = false
mathjax = true
categories = ['matlab']
tags = ['matlab', 'uiimage', 'ui']
toc = true
tocBorder = true
+++


## Matlab眼中的图形

图形在计算科学中扮演重要角色，毕竟，人人都和赌毒不共戴天。

在Matlab中，图形不过是一个平凡无奇的矩阵。大部分时候，图形都描述为$M \times N \times 3$的矩阵，其中$M$和$N$分别是图形的高和宽，3代表RGB三个通道。这种表示方法在Matlab中被称为`uint8`类型的图形。

```matlab
% 生成一个随机图形
img = randi([0, 255], 100, 100, 3, 'uint8');
imshow(img);
% 保存图形为PNG格式
exportgraphics(gca, '../matlab-img/rand-img.png');
```

![](/matlab-img/rand-img.png)


这里，我们看到每一个位置（也就是像素点）对应一个长度为3的向量，这三个数字代表了这个像素点的颜色。

Matlab提供了一堆函数来处理这样一个特殊的矩阵。

- imread：从文件中读取图形
- colormap：设置图形的颜色映射，这个函数确定Matlab如何将图形的数值映射到颜色
- imwrite：将图形保存到文件
- imshow：显示图形，通过`gcf`和`gca`获取当前的图形和坐标轴
- imagesc: 显示图形，但是不会自动调整坐标轴
- imfinfo: 获取图形的信息

## UIImage

在基于`uifigure`和App Designer的Matlab图形界面开发中，`UIImage`是一个常用的控件，用于显示图形。

```matlab
% 创建一个uifigure
fig = uifigure;

% 创建一个UIImage
img = imread('../matlab-img/rand-img.png');
uiimg = uiimage(fig, 'ImageSource', img);

% 输出图形界面元素到文件
exportapp(fig, '../matlab-img/app-rand-img.png')
```

![](/matlab-img/app-rand-img.png)

如果我们在这里把`img`存成一个`mat`文件。

```matlab
save('../matlab-img/rand-img.mat', 'img');
```

然后在App Designer中加载这个`mat`文件。

```matlab
% 创建一个uifigure
fig = uifigure;

% 从mat文件中加载图形
load('../matlab-img/rand-img.mat', 'img');

% 做一点小小的处理， 改变颜色
img = img * 0.5;

%
uiimg = uiimage(fig, 'ImageSource', img);

% 输出图形界面元素到文件
exportapp(fig, '../matlab-img/app-rand-img-mat.png')
```

颜色变深了好多。

![](/matlab-img/app-rand-img-mat.png)

就是这样做受益不太大，因为，`mat`文件的大小稍微会大一点。

```
-rw-r--r-- 1 qchen qchen 83629 Oct 26 09:38 static/matlab-img/rand-img.mat
-rw-r--r-- 1 qchen qchen 52028 Oct 26 09:26 static/matlab-img/rand-img.png
```

还可以把图形存成文本文件。

```matlab
writematrix(img, '../matlab-img/rand-img.txt');
```

再读取。

```matlab
img = readmatrix('../matlab-img/rand-img.txt');
```

这玩意就有点大的过分了……而且，读出来之后，还得转形状，因为3维数组在文本文件中是没法直接支持的。

```
-rw-r--r-- 1 qchen qchen   83629 Oct 26 09:38 static/matlab-img/rand-img.mat
-rw-r--r-- 1 qchen qchen   52028 Oct 26 09:26 static/matlab-img/rand-img.png
-rw-r--r-- 1 qchen qchen 2785443 Oct 26 09:42 static/matlab-img/rand-img.txt
```

实在在无聊的话，那就再搞点压缩算法，把这个数组好好压缩一下吧……

## 总结

1. 图形在Matlab中是一个矩阵
2. UIImage是Matlab图形界面开发中常用的控件
3. 通过`imread`和`imwrite`可以读取和保存图形
4. 图像也可以存储为`mat`文件，乃至文本文件，前者通过`save`和`load`可以保存和加载图形，后者通过`writematrix`和`readmatrix`可以保存和加载。