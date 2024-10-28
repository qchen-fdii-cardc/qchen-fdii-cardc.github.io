+++
title = 'Subplot_In_Matlab中多图绘制之subplot函数'
date = 2024-10-28T17:36:50+08:00
draft = true
mathjax = false
categories = ['matlab']
tags = ['matlab', 'tutorial', 'language', 'figure', 'subplot']
toc = true
tocBorder = true
+++


## 基于子图的多图方法

专业的论文中通常涉及到多个有逻辑关系的图拼接在一起，构成相互支持或者对照。所以很早之前，Matlab就有这个子图的函数`subplot`。

这个函数的基本语义有三类：

- 在图窗上划分出一个矩形区域建立一个坐标系，并指定该坐标系为当前坐标系
- 把一个坐标系替换成另外一个坐标系，并指定为当前坐标系
- 把一个坐标系指定为当前坐标系

因为Matlab的绘图指令可以忽略坐标系参数，并利用`gca`函数获取当前坐标系。所以当运行上述子图函数后，后续不指定坐标系的绘图指令就会在指定的坐标系上进行。

### 基于格子的多图方法




```matlab
% 基本的子图：利用subplot函数，并用函数语法画图
figure;
% 1. 画一个2x2的子图
subplot(2,2,1);
plot(1:10,1:10);
title('1');

subplot(2,2,2);
plot(1:10,1:10);
title('2');

subplot(2,2,3);
plot(1:10,1:10);
title('3');

subplot(2,2,4);
plot(1:10,1:10);
title('4');
```

    
![png](/matlab-img/multiple_axes_output_1_1.png)
    


简单的说，子图的编号采取了行先的方式，下面给出一个循环的例子，可以看到，图号按照`n * i + j`的方式编号，其中`n`是行数，`i`是行号，`j`是列号。


```matlab
figure;

m = 2;  % 行数
n = 3;  % 列数

for row = 1:m
    for col = 1:n
        idx = (row-1)*n + col;
        subplot(m,n,idx);
        plot(1:10,1:10);
        title(['子图编号' num2str(idx)]);
    end
end

```

    
    


    
![png](/matlab-img/multiple_axes_output_3_1.png)
    


实际上，子图可以拼凑在一起，比如下面这个例子中，把第一行的两个图绘制完成后；再创建一个两行一列的图，把第二行的子图作为一个子图。就形成了三个子图的图像。




```matlab
% 稍微复杂的子图：利用subplot函数，并用函数语法画图
figure;
% 1. 画一个2x2的子图
ax1 = subplot(2,2,1);
plot(ax1,1:10,1:10);
title(ax1,'1');

ax2 = subplot(2,2,2);
plot(ax2,1:10,1:10);
title(ax2,'2');

% 一个2x1的格子，横跨两列的子图
ax3 = subplot(2,1,2);
plot(ax3,1:10,1:10);
title(ax3,'3');
```

    
    


    
![png](/matlab-img/multiple_axes_output_5_1.png)
    


当然，从下面的例子可以看到，当有两个子图发生重叠时，就把先绘制的那个子图删除替换成新的子图。这里，先把第一行的两个子图绘制完成后，再绘制第二列的一个子图，这样就把第一行的第二个子图删除了。当然，左下角子图也可以顺利添加进来。


```matlab
% 稍微复杂的子图：利用subplot函数，并用函数语法画图
figure;
% 1. 画一个2x2的子图
ax1 = subplot(2,2,1);
plot(ax1,1:10,1:10);
title(ax1,'1');

ax2 = subplot(2,2,2);
plot(ax2,1:10,1:10);
title(ax2,'2');

% 一个1x2的格子，横跨两列的子图（删除了2,2,2对应的坐标系）
ax3 = subplot(1,2,2);
plot(ax3,1:10,1:10);
title(ax3,'3');

ax4 = subplot(2,2,3);
plot(ax4,1:10,1:10);
title(ax4,'4');
```

    
    


    
![png](/matlab-img/multiple_axes_output_7_1.png)
    


### 按照位置的多图方法

除了按照简单的行先编号的方式来创建子图坐标系，还可以直接设定子图的位置和大小还创建坐标系，这里就可以使用`Position`属性来设置子图，跟所有的`Position`属性一样，这里的位置包括了左下角的位置、宽度和高度。当然，这里采用的单位是归一化的单位，也就是左下角为`(0, 0)`，右上角为`(1, 1)`。



```matlab
figure;
ax1 = subplot("Position", [0.1 0.1 0.4 0.4]);
plot(1:10,1:10);
title('1');

ax2 = subplot("Position", [0.5 0.5 0.3 0.3]);
plot(1:10,1:10);
title('2');

ax3 = subplot("Position", [0.8 0.8 0.1 0.1]);
plot(1:10,1:10);
title('3');
```


   
![png](/matlab-img/multiple_axes_output_9_1.png)
    
这里有同样的问题，把上面脚本中子图坐标系的位置稍微调整一下就会发现，当有两个图重叠时，后绘制的图会把先绘制的图覆盖掉。

## 图中图的实现方法

当然，我知道你们在想什么，那么怎么绘制多个图，相互交叉（例如绘制部分放大图）呢？这就是图中图（画中画）的概念。要实现这个也很简单。

### 直接调整子图坐标系

那这样一想，我们能不能先分快创建子图坐标系，然后在调整`Position`属性来调整子图的位置呢？当然可以，下面的例子就是这样做的。


```matlab
figure;
ax1 = subplot(2,2,1);
plot(1:10,1:10);
title('1');

ax2 = subplot(2,2,2);
plot(1:10,1:10);
title('2');

ax3 = subplot(2,2,3);
plot(1:10,1:10);
title('3');

ax4 = subplot(2,2,4);
plot(1:10,1:10);
title('4');


ax1.Position = [0.15, 0.5, 0.5, 0.3];
ax2.Position = [0.75, 0.5, 0.2, 0.3];
ax3.Position = [0.1, 0.1, 0.8, 0.8];
ax3.Color = 'none';
```

    
    


    
![png](/matlab-img/multiple_axes_output_11_1.png)
    


### 创建子图坐标系
是否可以直接创建子图坐标系呢？当然可以，下面的例子就是这样做的。

只需要把`subplot`换成`axes`就可以，这个时候，就按照位置定位的方式来创建坐标系，通过把坐标系的背景颜色设置为`none`，就能把一个坐标系设定为透明。下面比较了是否设置为透明的效果。


```matlab
f = figure;
ax1 = axes("Position", [0.1 0.1 0.4 0.4]);
plot(1:10,1:10);
title('1');



ax2 = axes("Position", [0.45 0.45 0.3 0.3], 'Color', 'y');
plot(1:10,1:10);
title('2');

ax3 = axes("Position", [0.7 0.7 0.1 0.1], 'Color', 'g');
plot(1:10,1:10);
title('3');

ax4 = axes("Position", [0.2 0.2 0.7 0.7]);
x = 0:0.01:2*pi;
y = sin(x);
plot(x, y);
title('4');


ax1.Color = 'r';
ax2.Color = 'g';
ax3.Color = 'y';
ax4.Color = 'none';

exportgraphics(f, 'colors.png');
```

    
    


    
![png](/matlab-img/multiple_axes_output_13_1.png)
    


严肃一点，我们真正的做一个图中图，也就是对一个图的局部进行放大。


```matlab
x = 0:0.01:2*pi;
y = sin(x);

% 绘制一个图像
figure;
plot(x, y);
xlabel('x');
ylabel('y');
title('y = sin(x)');

% 增加一个坐标在pi/2处的放大图

ax1 = axes("Position", [0.6 0.6 0.2 0.2]);
idx = x >= pi/2-0.1 & x <= pi/2+0.1;
plot(x(idx), y(idx));
title('Zoomed view', Color='r');
xlim([pi/2-0.1, pi/2+0.1]);
ylim([0.9, 1.1]);

% 绘制一个箭头，指向放大区域
annotation('arrow', X=[0.58 0.35], Y=[0.75 0.9]);
annotation('rectangle', [0.285 0.9 0.05 0.05]);

```

    
    


    
![png](/matlab-img/multiple_axes_output_15_1.png)
    
这里比较烦人的就是要手动调整标注（箭头、方框）的位置，而且这个坐标是针对整个图的，不是针对子图的，也就是左小角坐标是`(0,0)`，右上角坐标`(1,1)`，宽度和高度的范围都是`[0, 1]`。


## 新的多图方法预告

从2022b开始就，一种新的、更加清晰的多图方法出现了。后面再写一个文章详细介绍这个方法。
