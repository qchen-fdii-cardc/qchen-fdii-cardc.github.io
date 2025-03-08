+++
title = 'Progress Dialog in Matlab中的进度条对话框'
date = 2024-11-14T08:48:46+08:00
draft = false
mathkatex = true
categories = ['matlab']
tags = ['matlab', 'gui', 'ux']
toc = true
tocBorder = true
+++


## 进度条

![](/matlab-img/pi-ing.png)

### 概念

在使用Matlab开发界面时，有一个很好用的工具就是进度条。在计算过程中，为用户提供计算进度的反馈是改善用户体验的重要手段。

一项进行的计算任务，如果其总体进度是比较容易量化，则可以按照0%~100%的方式，更新界面上的指示。



当一项任务的进度难以量化，或者只需要提示程序目前正在忙碌之中，则可以提供一个界面动画来提示任务正在进行中。

当然，进度条还可能提供另外一个功能，取消背景任务的功能。

综上所述，进度条的概念提供了三个方面的功能：

1. 显示任务的量化进度；
2. 显示非量化的任务进心中；
3. 提供取消任务的功能。

这在UI/UX中，是三个不同的概念。

此外，在使用进度条中，还有一个需要分析清楚的需求。也就是是否需要有并行的才做。这就决定在显示进度条的对话框（窗口）是模式对话框还是非模式对话框。

- 模式对话框：必须关闭对话框才能操作父窗口；
- 非模式对话框：无需关闭对话框，可以把焦点切换到父窗口，与其中的控件交互。

这两种模式的对话框用途略有不同，模式对话框采取了中断用户操作的方式，使用户的全部注意力集中于当前对话框中；而非模式对话框则是提供独立的额外信息，随时可以关闭、置于后台和查看。通过用户体验进行需求分析，很容易就可以确定使用哪种对话框。

- 如果程序只有一项中心任务正在进行，且不便于或者无需提供其它交互，则进度条选择模式对话框
- 如果是一项背景任务或者异步任务，这更适合选择非模式对话框来实现进度条，或者把进度条放置于当前窗口中的某个位置（如状态栏）

在Matlab这种以计算为核心的环境中，大部分时候都会把主要的资源集中用于数值计算，所以模式对话框来显示进度、提供取消的功能可能会更加符合逻辑。

### 工具

在以`uifigure`为基础的App开发中，提供了一系列对话框和通知的工具：


| 函数            | 作用                                           |
| --------------- | ---------------------------------------------- |
| `uialert`       | 显示警报对话框                                 |
| `uiconfirm`     | 创建确认对话框                                 |
| `uiprogressdlg` | 创建进度对话框                                 |
| `uisetcolor`    | 打开颜色选择器                                 |
| `uigetfile`     | 打开文件选择对话框                             |
| `uiputfile`     | 打开用于保存文件的对话框                       |
| `uigetdir`      | 打开文件夹选择对话框                           |
| `uiopen`        | 打开文件选择对话框并将选定的文件加载到工作区中 |
| `uisave`        | 打开用于将变量保存到 MAT 文件的对话框          |

## 进度条对话框

### 语法
进度条是一个对话框，名字都称为是`dlg`。调用的方式：

```matlab
d = uiprogressdlg(fig)
d = uiprogressdlg(fig,Name,Value)
```

调用的第一个参数是不能省略的，对应一个图窗，必须采用`uifigure`函数创建。

而参数有以下几类：

1. 外观：
   1. `Message`，显示在进度条上面的信息；
   2. `Title`，对话框的标题
   3. `Icon`，在进度条左侧显示的图标，采用图像名称或者图像数组
   4. `Interpreter`，文本的解释器（`tex`,`latex`,`html`），如果动态更新信息或者标题的频率过高，设置解释器可能会降低效率
2. 进度：
   1. `Value`，这个是一个$\in [0,1]$中的数值，就是主要的设定进度的接口
   2. `ShowPercentage`，是否显示比分比的文本，默认是'off'
   3. `Indeterminate`，设定不确定的进度，仅仅显示任务进行中
3. 交互性
   1. `Cancelable`，是否允许取消，如果设置为`on`，则显示一个取消按钮
   2. `CancelText`，设定取消按钮的文本
   3. `CancelRequest`，是否已经请求取消的属性，如果已经按下取消按钮，这个值就是`true`，程序可以选择相应的处理。

在创建过程中，可以设定这些值，也可以采用仅仅传入`fig`的方式调用，后续采用属性的方式设定。

```matlab
dlg = uiprogressdlg(fig);
dlg.Icon = "logo.png";
dlg.Cancelable = 'on';
dlg.CancelText = "Stop";

% ...

d.Message = "step 1 done";
d.Value = 0.55
% 更新进度的位置

```

这里还需要提一下`Cancelable`这个属性，如果这个属性设置为`false`，那么进度条对话框是没有关闭按钮的；如果这个属性设置为`true`，则进度条对话框有一个关闭按钮（窗体右上角），这个关闭按钮和取消按钮的作用完全一样。

### 示例

接下来就是一个例子，我们实现一个用蒙特卡洛采样计算$\pi$的程序，没啥用，效率低，仅仅是为了展示如何使用进度条对话框。


需求：

1. 用户输入：
   1. 蒙特卡洛采样次数
   2. 启动计算
2. 用户报表：
   1. 计算的采样次数
   2. 计算得到的$\pi$

经过设计，大概界面如下：

![](/matlab-img/pi.png)

点击计算之后，显示计算过程的信息。

![](/matlab-img/pi-ing.png)


```matlab
function f = calculationProgress

f = uifigure(Visible=false, Name="又一个计算圆周率的没用程序（更别说计算效率感人）");

g = uigridlayout(f, [3, 1], RowHeight={50, '1x', 100});

startBut = uibutton(g, Text="开始计算", ButtonPushedFcn={@startCalculation, f});
startBut.Layout.Row = 1;
startBut.Layout.Column = 1;

% line
g1 = uigridlayout(g, [1, 2], ColumnWidth={150, "1x"});
g1.Layout.Row=2;
g1.Layout.Column=1;

l = uilabel(g1);
l.Layout.Row = 1;
l.Layout.Column=1;

slider = uiknob(g1, Limit=[1, 100], Value=2, ValueChangedFcn={@setText, l});
slider.Layout.Row = 1;
slider.Layout.Column = 2;

setText(slider, [], l);

output = uilabel(g, Text="", Interpreter="latex", HorizontalAlignment='center', VerticalAlignment="center");
output.Layout.Row = 3;
output.Layout.Column = 1;

cm = uicontextmenu(f);
uimenu(cm, Text="拷贝圆周率", MenuSelectedFcn={@copyToClipboard, f});
output.ContextMenu = cm;


f.UserData = struct(...
    "piValue", nan, ...
    "startBut", startBut,...
    "output", output,...
    "slider", slider   );


f.Visible = true;
end
```

这里的界面布局用一个列布局（$N\times 1$网格）嵌套一个行布局（$1\times N$网格）的方式。整个界面的构造，非常直观。



### 计算和进度

按钮的动作调用回调函数`ButtonPushedFcn={@startCalculation, f}`，这里把`uifigure`传输到函数中，也作为`uiprogressdlg`的父节点，这就使得进度条对话框只能在图窗的范围内移动。

```matlab

function startCalculation(~, ~, fig)

output = fig.UserData.output;
startBut = fig.UserData.startBut;
startBut.Enable = false;

N = fig.UserData.slider.Value * 1024;
insideCount = 0.0;
idx = 0.0;

d = uiprogressdlg(fig, ...
    Icon="matlab.png", ...
    Title="计算中...", ...
    ShowPercentage=true, ...
    Cancelable=true,...
    CancelText="够了，够了!");

for i = 1:N
    if d.CancelRequested
        break;
    end
    x = rand;
    y = rand;
    idx = idx + 1;
    if (x * x + y * y < 1)
        insideCount = insideCount + 1;
    end
    estPi = 4 * insideCount / idx;
    fig.UserData.piValue = estPi;
    d.Value = idx / N;
    d.Message = sprintf("当前vs计划采样：%.0f / %.0f\n当前估计圆周率：%.8f", idx, N, estPi);
end

output.Text = sprintf("共采样：%.0f 次\n估计得：\\tilde{\\pi}=%.8f", idx, estPi);

startBut.Enable = true;

end
```

这里在循环内部，设定进度条的状态：

```matlab
d.Value = idx / N;
d.Message = sprintf("当前vs计划采样：%.0f / %.0f\n当前估计圆周率：%.8f", idx, N, estPi);
```

整个概念和使用都是极为直观的。


### 其它内容
上面的`uiknob`是一个模拟的旋钮。

回调函数中对应有一个旋钮的设置函数，`ValueChangedFcn={@setText, l}`，这个函数就把旋钮的值在旁边的`uilabel`上显示出来。

```matlab

function setText(src, ~, l)
l.Text = sprintf("设定采样次数: 1024*%.0f", src.Value);
end
```

这里还增加了一个把估计得到的$\pi$值拷贝到剪贴板的上下文菜单。这个上下文菜单中，`uicontextmenu`的父节点为一个图窗；`MenuSelectedFcn={@copyToClipboard, f}`是对应的动作回调；创建好之后，要把菜单与输出的标签相关联。

值得注意的是，这里的`uimenu`的父节点为`uicontextmenu`，如果其父节点是`uifigure`，则是窗口的菜单。


```matlab
cm = uicontextmenu(f);
uimenu(cm, Text="拷贝圆周率", MenuSelectedFcn={@copyToClipboard, f});
output.ContextMenu = cm;
```


这个回调函数非常简单，调用`clipboard('copy', piValue);`即可。

```matlab
function copyToClipboard(~, ~, f)
piValue = f.UserData.piValue;
% add value to copy/paste pad
clipboard('copy', piValue);
end

```

## 总结

这个可以取消的进度条，在实际使用中应该足够保守，仅仅用于那些长时间计算、并且在计算中不允许用户进行其它交互的场合。不然，过于激进地使用模式对话框还是很容易影响用户体验的。

