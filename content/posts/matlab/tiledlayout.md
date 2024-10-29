+++
title = 'Tiledlayout_in_Matlab中的分块图布局'
date = 2024-10-29T11:03:45+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'tiledlayout', 'tutorial', 'figure', 'subplot']
toc = true
tocBorder = true
+++





## 贴砖多图新方式

从R2019b开始，MATLAB提供了新的贴砖多图方式，可以更加方便的绘制多个子图。

这个功能由以下函数构成：

- `tiledlayout`
- `nexttile`
- `tilenum`
- `tilerowcol`

### `tiledlayout`和`TiledChartLayout`对象

`tiledlayout`函数用于创建一个贴砖布局，它对应于一个`TiledChartLayout`对象。调用语法有以下几种。

```matlab
tiledlayout(m,n)
tiledlayout(arrangement)
tiledlayout(___,Name,Value)
tiledlayout(parent,___)
t = tiledlayout(___)
```

分别对应的是：

- `m`和`n`：行数和列数，创建一个固定的行列布局
- `arrangement`：自动调节布局方式，可以是`"flow"`或`"vertical"`，`"horizontal"`，新增加的行列数会根据子图的大小自动调整
- 设置`Name-Value`对，可以设置`"Padding"`，`"TileSpacing"`，`"TileWidth"`，`"TileHeight"`等属性
- `parent`：指定父级对象
- `t`：返回一个`TiledChartLayout`对象


### `nexttile`函数

`nexttile`函数用于在贴砖布局中创建下一个子图，调用语法有以下几种。

```matlab
nexttile
nexttile(span)
nexttile(tilelocation)
nexttile(tilelocation,span)
nexttile(parent_tiledlayout, ___)
ax = nexttile(___)
```

这个函数在`TiledChartLayout`对象上调用，返回一个`Axes`对象。这个对应的坐标轴对象可以用于绘图，并且会被设定为当前坐标轴，a.k.a.，`gca`返回这个坐标系，a.k.a.，可以直接调用`plot`等函数进行绘图。

- `nexttile`：在下一个位置创建一个子图
- `nexttile(span)`：在下一个位置创建一个子图，占据`span`描述的区域，`span`是一个二元向量，分别表示行数和列数
- `nexttile(tilelocation)`：在指定位置创建/更新一个子图，`tilelocation`是一个数字，表示位置（按顺序数过来）
- `nexttile(tilelocation,span)`：在指定位置创建/更新一个子图，占据`span`描述的区域
- `nexttile(parent_tiledlayout, ___)`:可以使用前面所有的语法，但是设定父级对象，一般而言，总是在图窗中查找父级对象，当把`TiledChartLayout`对象放在其他容器中时，需要指定父级对象
- `ax = nexttile(___)`：返回之前创建的`Axes`对象，可以使用前面所有的语法调用

另外，有一个很没有一致性的地方，`tilelocation`是从左上角开始数的，行先的方式；并且还可以设定为`"east"`，`"west"`，`"north"`，`"south"`等方位，分别表示东西南北的网格外层的图块……此时，我的表情是……

        
![png](/matlab-img/tiledlayout_output_2_1.png)

```matlab
t = tiledlayout(2,2);

t.Title.String = 'Shared Title';
t.Subtitle.String = 'Shared Subtitle';
t.XLabel.String = 'Shared X-axis';
t.YLabel.String = 'Shared Y-axis';

nexttile
plot(1:10, rand(1,10));
title('1st Tile');

nexttile
plot(1:10, rand(1,10));
title('2nd Tile');

nexttile
plot(1:10, rand(1,10));
title('3rd Tile');

nexttile
plot(1:10, rand(1,10));
title('4th Tile');

nexttile('east')
plot(1:10, rand(1,10));
title('East');

nexttile('south')
plot(1:10, rand(1,10));
title('South');

nexttile('west')
plot(1:10, rand(1,10));
title('West');

nexttile('north')
plot(1:10, rand(1,10));
title('North');
```

    

    


### `tilenum`和`tilerowcol`函数

这两个函数就是用来获取当前的行列数和位置的转换。


```matlab
[m, n] = deal(3, 4);
t = tiledlayout(m, n);

for i = 1:tilenum(t, m, n)
    nexttile
    [row, col] = tilerowcol(t, i);
    plot(1:10, rand(1,10));
    title(['T' num2str(i), '-(', num2str(row), ',', num2str(col), ')']);
end


t.Title.String = sprintf('%d,', tilenum(t, [1 1 1 1 2 2 2 2 3 3 3 3], [1 2 3 4 1 2 3 4 1 2 3 4]));
```

    
    


    
![png](/matlab-img/tiledlayout_output_4_1.png)
    


## 行列布局

采取`tiledlayout(m,n)`的方式创建一个行列布局，然后使用`nexttile`函数来创建子图。

与其他一般的图形中会使用字符串来设置标签不同，这里的标题（`Title`），副标题（`Subtitle`），X轴标签（`XLabel`），Y轴标签（`YLabel`）等属性都是直接设置为`Text`对象。而且这几个对象都是整个布局共享的。示例如下。

确定图形周围空白大小和图块间距的两个参数分别是：

- `Padding`：图形周围的空白大小，`'loose'`, `compact`, `'tight'`，默认是`'loose'`
- `TileSpacing`：图块间距，`'loose'`, `'compact'`, `'tight'`, `'none'`，默认是`'loose'`



```matlab
t = tiledlayout(2, 2);

t.TileSpacing = 'tight';
t.Padding = 'compact';

t.Title.String = 'Random samples';
t.XLabel.String = 'Random Count';
t.YLabel.String = 'Random Value';
t.Subtitle.String = '4 independent random samples';

nexttile
stem(1:10, rand(1, 10));

nexttile
stem(1:10, rand(1, 10));

nexttile
stem(1:10, rand(1, 10));

nexttile
stem(1:10, rand(1, 10));
```

    
    


    
![png](/matlab-img/tiledlayout_output_6_1.png)
    


## 流式、单行、单列布局

采用`tiledlayout("arrangement")`的方式创建一个流式布局，然后使用`nexttile`函数来创建子图。这里的`"arrangement"`可以是`"flow"`，`"vertical"`，`"horizontal"`，新增加的行列数会根据子图的大小自动调整。

注意，在这种情况下，可以采用命令的方式来调用：


```matlab
tiledlayout flow

nexttile
plot(1:10, rand(1, 10));
```

    
    


    
![png](/matlab-img/tiledlayout_output_8_1.png)
    


增加一个新的图。


```matlab
tiledlayout flow

nexttile
plot(1:10, rand(1, 10));

nexttile
plot(1:10, rand(1, 10));
```

    
    


    
![png](/matlab-img/tiledlayout_output_10_1.png)
    


再增加一个新的图。


```matlab
tiledlayout flow

nexttile
plot(1:10, rand(1, 10));

nexttile
plot(1:10, rand(1, 10));

nexttile
plot(1:10, rand(1, 10));
```

    
    


    
![png](/matlab-img/tiledlayout_output_12_1.png)
    


观察这里的布局方式，流式布局总是试图维持大概的长宽比例来适应子图的大小。

相应的，垂直布局和水平布局也很好理解。

下面试试，行列布局的嵌套。


```matlab
t = tiledlayout('vertical');

t2 = tiledlayout(t, 'horizontal');

nexttile(t)
plot(1:10, rand(1, 10));

nexttile(t)
plot(1:10, rand(1, 10));

nexttile(t2)
plot(1:10, rand(1, 10));

nexttile(t2)
plot(1:10, rand(1, 10));

nexttile(t2)
plot(1:10, rand(1, 10));
```

    
    


    
![png](/matlab-img/tiledlayout_output_14_1.png)
    


或者是这样嵌套。

这里建立一个嵌套的列之后，显式地指定了在父布局中的位置。
其中`Layout.Tile`属性指定了位置，第二个位置；`Layout.TileSpan`则指定了相应区域大小，也就是这里的两列。

最终结果是这样的。


```matlab
t = tiledlayout('horizontal');

nexttile(t)

plot(1:10, rand(1, 10));

t2 = tiledlayout(t, 'vertical');
t2.Layout.Tile = 2;
t2.Layout.TileSpan = [1 2];

nexttile(t)
plot(1:10, rand(1, 10));

nexttile(t2)
plot(1:10, rand(1, 10));

nexttile(t2)
plot(1:10, rand(1, 10));

nexttile(t2)
plot(1:10, rand(1, 10));


```

    
    


    
![png](/matlab-img/tiledlayout_output_16_1.png)
    


## 地理图+极坐标系

可以看到，`nexttile`只是占了一个位置，实际的坐标系是可以自由设置的。

下面给出一个地理图和极坐标系的例子。


```matlab
titleX = matlab.graphics.layout.Text(String="标题不能胡说",Color='blue');
subtitleX = matlab.graphics.layout.Text(String="更不能细说",Color='red');


tiledlayout(1,2, Title=titleX, Subtitle=subtitleX);

% Display geographic plot
nexttile
geoplot([22.6 24.9 30.5 39.1 41.9],[113.6  120.6 120.5 117.2 123.6],'r-*')

% Display polar plot
nexttile
theta = pi/4:pi/4:2*pi;
rho = [19 6 12 18 16 11 15 15];
polarscatter(theta,rho)
```

    
    


    
![png](/matlab-img/tiledlayout_output_18_1.png)
    


## 总结

这个方式比之前的`subplot`函数更加灵活，可以更加方便的绘制多个子图。但是，这个功能在R2019b之后才有，所以需要注意版本的问题。
