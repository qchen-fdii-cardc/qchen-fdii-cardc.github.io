+++
title = 'Uncertainty Chart自定义绘图之误差图'
date = 2025-04-18T14:38:13+08:00
draft = true
mathkatex = true
categories = ['matlab', 'charting']
tags = ['matlab', 'charting', 'errorbar', 'confidence chart', 'gaussian process regression']
toc = true
tocBorder = true
+++

## Matlab绘图

Matlab的用途分为两半，一半是计算，一半是绘图。

虽然现在Python中的各种绘图工具越来越多，Matplotlib的质量和能力很不错，各种交互式图更是Python的强项。但是Matlab的图表绘制功能，依然有让很多~~学不会~~不会学Python的工程师和科研工作者天天都要用。

Matlab的绘图包，整个看起来也没啥设计感，就是那种很丑的工业风，怼到哪里算哪里。到底使用参数、命名参数还是直接在函数里写死，全看心情。还好呢，Matlab中的图和图标对象还靠点面向对象，提供了各种UI上交互式调整的接口，不然可真是要愁死我们这些不会编程只会鼠标点点点的傻瓜们。

这里还可以插个题外话，Matlab的UI部分，一直是以来Java的体系，就比如

```matlab
f = figure();
f.JavaFrame
%   Warning: The JavaFrame figure property will be removed in a future release. For
%   more information see UI Alternatives for MATLAB Apps on mathworks.com. 
%   ans =
%       com.mathworks.hg.peer.HG2FigurePeer@319c3a25
```

从这里可就能看到，实际上，每个图窗有个不公开的属性，叫做`JavaFrame`，就是Java中Swing的类似于`JFrame`的东西。当然，最近的更新之后，Matlab的UI要逐步转向基于Web技术的体系，所以这个`JavaFrame`的属性，目前已经确定是要逐步淘汰。

```matlab
f2 = uifigure();
f2.JavaFrame
%   Warning: The JavaFrame figure property will be removed in a future release. For
%   more information see UI Alternatives for MATLAB Apps on mathworks.com. 
%   ans =
%     []
```

这个属性在新的UI开发工具`uifigure`中，已经不存在了。如果我们用`class`来查看这两个对象，就会发现还都是`matlab.ui.Figure`。

当然，目前我们大部分的绘图函数，其输出都是在`figure`对象中，所以这个`JavaFrame`的属性，目前还是存在的。

## 误差图

误差图是Matlab中非常常用的一种图表，用于表示数据的不确定性。在Matlab中，误差图可以通过`errorbar`函数来绘制。

```matlab
x = 1:10;
y = randn(1,10);
errorbar(x,y,std(y));
```

![Error Bar](/matlab/charting/uncertainty-chart/eb1.png)

```matlab
which errorbar
%   C:\Program Files\MATLAB\R2023b\toolbox\matlab\specgraph\errorbar.m
edit errorbar
```

这个函数的大部分工作都在预处理输入参数什么的，最终，核心的创建误差图的代码，是下面这样子的：

```matlab
% Create the ErrorBar objects
h = gobjects(1,numSeries);
xdata = {};
for n = 1:numSeries

    stylepv={};
    if ~isempty(cax)
        [ls,c,m] = matlab.graphics.chart.internal.nextstyle(cax,autoColor,autoStyle,true);
        stylepv={colorPropName c stylePropName ls 'Marker_I',m};
    end

    if ~autoXData
        xdata = {'XData', x(:,n)};
    end

    h(n) = matlab.graphics.chart.primitive.ErrorBar('Parent',parax, ...
        'YData',y(:,n),xdata{:},...
        'XNegativeDelta',xneg(:,n),...
        'XPositiveDelta',xpos(:,n),...
        'YNegativeDelta',yneg(:,n),...
        'YPositiveDelta',ypos(:,n),...
        stylepv{:},...
        pvpairs{:},...
        extrapairs{n,:});
    h(n).assignSeriesIndex();
end

if ~hold_state
    set(cax,'Box','on');
end

if nargout>0, hh = h; end

end
```

注意到，`errorbar`函数最终创建的是`matlab.graphics.chart.primitive.ErrorBar`对象。

```matlab
which matlab.graphics.chart.primitive.ErrorBar
%   C:\Program Files\MATLAB\R2023b\toolbox\matlab\specgraph\+matlab\+graphics\+chart\+primitive\@ErrorBar\ErrorBar.p  % matlab.graphics.chart.primitive.ErrorBar constructor
```

这个对象是已经编译成P-Code的，所以不能直接查看和编辑。

## 不确定度

这个图挺好的，但是我们想绘制一个更个性化的图，比如：

![Confidence Chart](/matlab/charting/uncertainty-chart/cc1.png)

当我们采用Gaussian Process Regression（高斯过程回归）时，我们通常会得到一个预测的均值和方差，我们可以将这个方差作为误差图的误差值，绘制这样的图。

![Confidence Chart](/matlab/charting/uncertainty-chart/cc2.png)

![Confidence Chart](/matlab/charting/uncertainty-chart/cc3.png)
