+++
title = 'Position_Layout_in_Matlab界面布局之设定位置'
date = 2024-10-20T23:33:45+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'uifigure', 'position', 'layout', 'ui']
toc = true
tocBorder = true
+++


## 抽象整活

前面我们对利用`uigridlayout`和一些容器组件来布局`uifigure`界面进行了一番讨论，参考[UI_Layout_in_MATLAB中APP界面布局容器](/posts/matlab/ui-layout/). 通过每个容器放一个网格布局组件的方式，所有的界面布局都由网格布局器来完成，其宽度、高度通过网格布局器来设置为：

- 固定值： 固定数值
- 可变值: `1x`, `2x`，等
- 自适应: `fit`

通过这样的布局方式，能够用较少的代码和心智负担实现相对来说比较漂亮和规整的界面布局。

这样的布局方式只有一个缺点，就是失去了抽象整活的程序员灵魂……这样的界面是没有灵魂的~~~

那么，我们这里再讨论一下另外一种布局方式，即通过设置组件的位置来布局`uifigure`界面。

## 位置

首先，在`uifigure`技术路线中，用一个属性`Position`来设置组件的位置。`Position`属性是一个四元素的向量，分别表示组件的左下角的x坐标、y坐标、宽度和高度，`[left, bottom, width, height]`。这里的坐标是相对于父容器的坐标。

| 属性 | 描述 |
| --- | --- |
| left | 组件左下角的x坐标 |
| bottom | 组件左下角的y坐标 |
| width | 组件的宽度 |
| height | 组件的高度 |

对于`uifigure`界面中的`Children`，`Position`属性的值是相对于`uifigure`左下角，例如左下角的坐标放置于`uifigure`左下角，则从`Position(1:2)=[0, 0]`。

### `uifigure`的`Position`属性

首先，我们可以考虑设置`uifigure`的`Position`属性，这样可以设置`uifigure`的位置和大小。例如：

```matlab
f = uifigure('Position', [100, 100, 400, 300]);
```

那么，`uifigure`的左下角的坐标是`(100, 100)`，宽度是`400`，高度是`300`。

Matlab提供了一个比较方便的函数`movegui`，可以用来移动`uifigure`的位置，例如：

```matlab
movegui(f, 'center');
```

这个函数嗲用方式是`movegui(f, position)`，其中`position`可以是：

- `center`: 将`uifigure`移动到屏幕中心
- `north`: 将`uifigure`移动到屏幕上方
- `south`: 将`uifigure`移动到屏幕下方
- `east`: 将`uifigure`移动到屏幕右侧
- `west`: 将`uifigure`移动到屏幕左侧
- `northeast`: 将`uifigure`移动到屏幕右上角
- `northwest`: 将`uifigure`移动到屏幕左上角
- `southeast`: 将`uifigure`移动到屏幕右下角
- `southwest`: 将`uifigure`移动到屏幕左下角
- `onscreen`: 将`uifigure`移动到屏幕上
- `center`: 将`uifigure`移动到屏幕中心
- [left, bottom]: 将`uifigure`移动到指定的位置

### UI组件的`Position`属性以及位置

对于`uifigure`中的`Children`，可以通过设置`Position`属性来设置组件的位置。例如：

```matlab
b = uibutton(f, 'Position', [100, 100, 100, 50], 'Text', 'Button');
```

这个按钮的左下角的坐标是`(100, 100)`，宽度是`100`，高度是`50`。

当然，很容易想到，对于设置位置的组件，可能存在组件相互重叠的问题。所以组件的位置，还应该有一个维度，即`Z`维度，表示组件的层次。`Z`维度越大，组件越靠前。对于`uifigure`中的`Children`，可以通过`uistack`函数来设置组件的`Z`维度，例如：

```matlab
uistack(b, 'top');
uistack(b, 'up', 1);
uistack(b, 'down', 1);
uistack(b, 'bottom');
```

这里面，方向和步长的默认值是`'up', 1`，即`uistack(b)`将组件`b`向上移动一个单位。

## 具体的抽象

下面，我们通过一个具体的例子来说明如何通过设置组件的位置来布局`uifigure`界面。

### 例子

```matlab
function fig = uiPositionLayout()

    % Follow the convention to return the figure object
    % Since uifigure with "HandleVisibility" set to "off" is not accessible by gcf
    fig = uifigure('Visible', 'off');

    %  update window size, width x height
    fig.Position(3:4) = [800 600];
    % update widnow position with respect to the screen
    movegui(fig, 'center');

    % Add three images to the figure
    peppers = uiimage(fig);
    peppers.ImageSource = "peppers.png";
    peppers.Position(1:2) = [10 300];

    street = uiimage(fig);
    street.ImageSource = "street1.jpg";
    street.Position(1:2) = [230 250];

    nebula = uiimage(fig);
    nebula.ImageSource = "ngc6543a.jpg";
    nebula.Position(1:2) = [150 180];

    for handle = [peppers, street, nebula]
        fprintf("Image source: %s\n", handle.ImageSource);
        handle.Position(3:4) = handle.Position(3:4) * 3;
        handle.ImageClickedFcn = @mouseClicker;
        handle.Tooltip = sprintf("%s: (left, bottom, width, height)= %d, %d, %d, %d", handle.ImageSource, handle.Position);
    end

    % set Position to randomize image position
    btn = uibutton(fig);
    btn.Position = [10 70 145 22];
    btn.Text = "Random Position";

    spn = uispinner(fig);
    spn.Position = [165 70 125 22];
    spn.Limits = [1 3];

    ta = uitextarea(fig);
    ta.Position = [10 10 280 50];

    btn.ButtonPushedFcn = @(~, ~) randomMize([peppers, street, nebula], ta, spn);

    fig.UserData = struct("peppers", peppers, "street", street, "nebula", nebula, "btn", btn, "spn", spn, "ta", ta);
    fig.Visible = 'on';
end

% Callback function for the button
function randomMize(imgs, outputArea, choice)
    idx = choice.Value;
    img = imgs(idx);
    img.Position(1:2) = randi([10 500], 1, 2);
    outputArea.Value = sprintf("New position for %s: %d, %d\n", img.ImageSource, img.Position(1), img.Position(2));
    img.Tooltip = sprintf("%s: (left, bottom, width, height)= %d, %d, %d, %d", img.ImageSource, img.Position);
end

% Callback function for the image
function mouseClicker(src, ~)
    uistack(src, "up");
end
```

[uiPositionLayout.m](/matlab-code/uiPositionLayout.m)

### 效果

这个抽象的App显示了三个图片，一个按钮，一个下拉框和一个文本框。通过点击按钮，可以随机移动其中一个图片的位置，通过下拉框来选择图片（1，2，3），移动的信息显示在文本框中。通过点击图片，可以将图片前置1层。

![uiPositionLayout](/matlab-img/uiPositionLayout.png)


## 细节和惯例

这个例子中，有几个细节值得稍微提一下。

### 函数参数与返回值

在我们编程实现App时，通常会遵循一些惯例，例如：

- 作为一个函数来实现App
- 函数的返回值是`uifigure`对象
- 函数没有参数

这几个惯例的好处是：

- 关闭App时，Matlab会自动释放资源
- 通过返回值，可以在函数外部访问`uifigure`对象
- 函数没有参数，或者用默认参数来实现App的初始化，更好地封装了App的实现，便于打包和发布


### `Visible`属性

通常，我们会将`uifigure`的`Visible`属性设置为`'off'`，这样可以在App初始化时，不显示App界面，等App初始化完成后，再将`Visible`属性设置为`'on'`，这样可以避免界面闪烁，提高用户体验。

### `UserData`属性

`UserData`属性是一个很方便的属性，可以用来存储一些数据，例如在这个例子中，我们将图片、按钮、下拉框和文本框的句柄存储在`UserData`属性中，这样可以在回调函数中方便地访问这些句柄。通常，这个`struct`结构体可以存储一些App的状态信息，方便在回调函数中访问。

### 回调函数

在这个例子中，我们通过回调函数来实现按钮的功能，通过回调函数来实现图片的功能。回调函数是一个很方便的机制，可以实现用户交互功能。

回调函数的定义方式有很多种，例如：

- 匿名函数
- 内部函数
- 嵌套函数

通常，我喜欢用在内部函数的基础上用匿名函数来捕获一些状态信息，例如这个例子中，我们通过匿名函数来实现按钮的功能，通过内部函数来实现图片的功能。

当然，我们也可以直接捕获整个`uifigure`对象，然后在回调函数中访问`uifigure`对象的`UserData`属性，来实现功能。这在有些情况下能够获得一些收益。


## 总结

- `Position`属性对于图形对象来说是一个通用的属性，包括左下角坐标、宽度和高度
- 通过设置`Position`属性，可以实现`uifigure`界面的布局
- 通过`uistack`函数，可以设置组件的`Z`维度，来实现组件的层次
- 回调函数是实现用户交互功能的重要机制