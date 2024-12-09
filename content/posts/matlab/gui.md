+++
title = 'GUI_design_in_Matlab中的界面开发'
date = 2024-10-21T16:22:04+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'gui', 'uifigure', 'app designer']
toc = true
tocBorder = true
+++

- [GUI系列](/tags/ui/)


## Matlab的GUI设计

以前，Matlab有一个GUIDE工具，可以用来设计GUI，凑凑活活，要说毛病，主要是逻辑上很难写清楚，大量工具都没有提供，我记得好像数据交换通过控件的Tag属性来整（没做过几个，早就忘记）。在2019b，MathWorks声明未来会停止发布并删除GUIDE，并且提供了新的开发工具和迁移工具。从2020a开始，GUIDE ==> App Designer的迁移工具可用性大大增强，已经很好用。

目前，Matlab的GUI设计有两种方式：

- App Designer拖放式设计
- 编程设计GUI

### App Designer
![App Designer Entry](/matlab-img/appdesigner.png)

App Designer的入口在主界面的工具栏，在`APP`标签下面，第一个按钮就是App Designer。App Designer是一个拖放式设计工具，可以很方便的设计GUI。

App Designer的界面跟QtDesigner，SceneBuilder等界面设计工具类似，左边是控件列表，中间是设计区，右边是属性编辑器。设计区可以拖放控件，属性编辑器可以编辑控件的属性。

同样，App Designer有两种模式来查看GUI，一种是设计模式（GUI的显示），一种是代码模式（是一个类，可以编辑GUI的回调函数）。

在第一个模式下，可以拖放控件，设置控件属性。

![在这里插入图片描述](/matlab-img/appdesigner-gui.png)




在第二个模式下，可以编辑GUI的回调函数，这个模式下，可以看到GUI的类定义，可以编辑类的方法。

![在这里插入图片描述](/matlab-img/appdesigner-codeview.png)

这种方式简单直观，封装也很不错，适合比较简单地给编的函数加个GUI。设计好的APP可以导出成一个独立的APP，可以在没有Matlab的机器上运行（需要安装对应的runtime），也能导出成一个m文件，可以在Matlab命令行运行。

整个过程很简单，按照操作一步一步尝试就行。MathWorks还提供Runtime的下载，可供客户安装。
![在这里插入图片描述](/matlab-img/appdesigner-compiler.png)

这个功能也可以通过Matlab主界面的`APPS`标签下的`Application Compiler`来实现，这个功能可以把Matlab程序打包成一个独立的程序，不需要安装Matlab就可以运行。命令行下也有对应的`mcc`命令。


### 编程设计GUI

GUI编程实现，可以用`figure`作为窗口，也可以用`uifigure`作为窗口，这里先介绍`uifigure`的方式。这个方式提供与App Designer类似的功能，但是通过一系列`ui`开头的函数来实现。

所涉及的函数可以做如下的总结。

#### 容器

| 函数           | 说明            |
| -------------- | --------------- |
| `uifigure`     | 创建GUI设计窗口 |
| `uigridlayout` | 创建网格布局    |
| `uipaenl`      | 创建面板        |
| `uitabgroup`   | 创建标签页      |
| `uitab`        | 创建标签        |

#### 坐标控件

| 函数        | 说明             |
| ----------- | ---------------- |
| `uiaxes`    | 创建画图的坐标系 |
| `axes`      | 创建笛卡尔坐标系 |
| `geoaxes`   | 创建地理坐标系   |
| `polaraxes` | 创建极坐标系     |

#### 通用控件

| 函数             | 说明                                     |
| ---------------- | ---------------------------------------- |
| `uibutton`       | 创建按钮                                 |
| `uibuttongroup`  | 创建按钮组（特别是复选框按钮和单选按钮） |
| `uicheckbox`     | 创建复选框                               |
| `uidatepicker`   | 创建日期选择控件                         |
| `uidropdown`     | 创建下拉式列表                           |
| `uieditfield`    | 创建编辑框                               |
| `uihyperlink`    | 创建超链控件                             |
| `uiimage`        | 创建图片控件                             |
| `uilabel`        | 创建文本标签控件                         |
| `uilistbox`      | 创建列表控件                             |
| `uiradiobutton`  | 创建单选按钮                             |
| `uislider`       | 创建滑动条                               |
| `uispinner`      | 创建旋转扭                               |
| `uitable`        | 创建表格                                 |
| `uitextarea`     | 创建文本域                               |
| `uitogglebutton` | 创建状态开关按钮                         |
| `uitree`         | 创建标准树和复选框树                     |
| `uitreenode`     | 创建树节点                               |

#### 图形工具

| 函数            | 说明               |
| --------------- | ------------------ |
| `uicontextmenu` | 创建上下文菜单     |
| `uimenu`        | 创建菜单           |
| `uipushtool`    | 创建工具栏按钮     |
| `uitoggletool`  | 创建工具栏开关按钮 |
| `uitoolbar`     | 创建工具栏         |

#### 仪器外观控件

| 函数       | 说明       |
| ---------- | ---------- |
| `uigauge`  | 创建仪表盘 |
| `uilamp`   | 创建灯泡   |
| `uiknob`   | 创建旋钮   |
| `uiswitch` | 创建开关   |

#### 可扩展控件

| 函数     | 说明         |
| -------- | ------------ |
| `uihtml` | 创建HTML控件 |

#### 控件控制与风格

| 函数          | 说明               |
| ------------- | ------------------ |
| `expand`      | 展开树节点         |
| `collapse`    | 折叠树节点         |
| `move`        | 移动控件           |
| `scroll`      | 滚动控件           |
| `open`        | 打开上下文相关菜单 |
| `uistyle`     | 创建控件样式       |
| `addStyle`    | 添加控件样式       |
| `removeStyle` | 移除控件样式       |

#### 对话框和消息通知

| 函数            | 说明                  |
| --------------- | --------------------- |
| `uialert`       | 创建警告对话框        |
| `uiconfirm`     | 创建确认对话框        |
| `uiprogressdlg` | 创建进度对话框        |
| `uisetcolor`    | 创建颜色选择对话框    |
| `uigetfile`     | 创建文件选择对话框    |
| `uigetdir`      | 创建文件夹选择对话框  |
| `uiputfile`     | 创建文件保存对话框    |
| `uiopen`        | 创建载入文件对话框    |
| `uisave`        | 创建保存mat文件对话框 |


### 总结

具体所需要的函数除了上面的，主要是各种对象的属性，这个具体查询文档即可。

## 一个简单的例子

我们的要求是，设计一个UI输入一组参数，这组参数每个都有一个名字，每个都有一个单位，每一个都有一个默认值，要求按照单位输入数字，最终得到一个数组，这个数组就是输入的参数。

### 需求

分析需求，也就是编写测试用例。

```matlab
labels = {'a', 'b', 'c'};
defaultValues = [1,2,3];
units = {'m/s', 'kg', '$m^2$'};

% 测试用例一，包含单位输入，第一个参数是矿口标题，第二参数是参数名，第三个参数是默认值，第四个参数是单位
parameters = editParameters('test', labels, defaultValues, units);
disp(parameters);

% 测试用例二，不输入单位，所有参数均为无量纲，第一个参数是矿口标题，第二参数是参数名，第三个参数是默认值
parameters = editParameters('test', labels, defaultValues);
disp(parameters);
```

### 设计

确定测试用例，就能直接开始编写代码。

```matlab
function parameters = editParameters('test', labels, defaultValues, units)

parameters = defaultValues;

```

这个函数就能让测试一无报错运行，测试二不行。那么先修理测试二。

```matlab
function parameters = editParameters('test', labels, defaultValues, units)
n = length(labels);
if nargin < 4
    units = num2cell(repmat("", 1, n));
end

parameters = defaultValues;

```

这样可以通过两个测试，接下来就是设计UI。

首先，我们对输入参数的特性进行规范，也称为调用规范。

```matlab
assert(iscell(labels) , 'labels must be a cell array');
assert(iscell(units), 'units must be a cell array');
assert(isnumeric(defaultValues), 'defaultValues must be a numeric array');
assert(n == length(defaultValues), 'labels and defaultValues must have the same length');
assert(n == length(units), 'labels and units must have the same length');    
```

这段代码规范输入参数类型、长度等，这样可以确保输入参数正确。

接下来，我们设计UI。

```matlab
% Create a figure and axes
fig = uifigure('Position',[100 100 600 (n+1)*50]);
fig.Name = sprintf("%s (n=%d)", name, n);

layout = uigridlayout(fig);

layout.ColumnWidth = {'4x', '8x', '4x'};
layout.RowHeight = [repmat("1x", 1, n), "1.2x"];

```

这段代码创建窗口，标题为输入参数，位置为100,100，大小为600x(n+1)*50，其中n为参数个数。然后创建一个网格布局，设置列宽和行高的比例关系。

接下来，我们创建控件。

```matlab
edits = cell(1, n);

for i = 1:n
    label = uilabel(layout);
    label.Text = labels{i}+":";
    label.Layout.Row = i;
    label.Layout.Column = 1;
    label.HorizontalAlignment = 'right';
    
    edit = uieditfield(layout, 'numeric');
    edit.Layout.Row = i;
    edit.Layout.Column = 2;
    edit.Value = defaultValues(i);
    
    edits{i} = edit;
    
    unit = uilabel(layout);
    unit.Text = units{i};
    unit.Interpreter = 'latex';
    unit.Layout.Row = i;
    unit.Layout.Column = 3;
    
    edit.ValueChangedFcn = @(~,~)editValueChangedFcn(i, edit.Value);
end
```

对于每一个参数，创建一个标签，一个编辑框，一个单位标签，然后设置位置，值，单位等。最后，把编辑框的句柄保存下来，并设置编辑框的值变化回调函数。

接下来是按向量方式输入的输入框，两个按钮（重置为默认值、确定）
```matlab
vectorLabel = uilabel(layout);
vectorLabel.Text = "向量形式:";
vectorLabel.Layout.Row = n+1;
vectorLabel.Layout.Column = 1;
vectorLabel.HorizontalAlignment = 'right';

vectorEdit = uieditfield(layout, 'text');
vectorEdit.Layout.Row = n+1;
vectorEdit.Layout.Column = 2;
vectorEdit.Value = mat2str(reshape(defaultValues, 1, n));
vectorEdit.ValueChangedFcn = @(~,~)updateAllValues(vectorEdit.Value);

buttonLayout = uigridlayout(layout);
buttonLayout.ColumnWidth = {'1x', '1x'};
buttonLayout.RowHeight = {'1x'};
buttonLayout.Layout.Row = n+1;
buttonLayout.Layout.Column = 3;

resetBut = uibutton(buttonLayout);
resetBut.Text = "重置";
resetBut.Layout.Row = 1;
resetBut.Layout.Column = 1;

okBut = uibutton(buttonLayout);
okBut.Text = "确定";
okBut.Layout.Row = 1;
okBut.Layout.Column = 2;

resetBut.ButtonPushedFcn = @(~,~)resetValues();
okBut.ButtonPushedFcn = @(~,~)okButFcn();
fig.KeyPressFcn = @(~,evt)keyPressFcn(evt);
fig.CloseRequestFcn = @(~,~)okButFcn();


uiwait(fig);
```

这段代码创建标签、编辑框来输入向量形式数据，最后两个按钮共占网格第三列，分别是重置和确定。重置按钮的回调函数是重置所有参数为默认值，确定按钮的回调函数是关闭窗口。

最后这个`uiwait(fig)`是阻塞窗口，直到窗口关闭才会继续执行，相当于模态对话框，否则就是无模态对话框。可以删除这句试试结果。

除两个按钮的事件、向量编辑事件，最后还给整个窗口设置键盘事件和关闭事件，回调函数如下。

```matlab
    function updateAllValues(val)
        matValue = str2num(val); %#ok<ST2NM>
        if length(matValue)>= n
            arrayfun(@(i)editValueChangedFcn(i, matValue(i)), (1:n));
        end
    end

    function resetValues()
        arrayfun(@(i)editValueChangedFcn(i, defaultValues(i)), (1:n));
    end
    function editValueChangedFcn(i, value)
        if edits{i}.Value ~= value
            edits{i}.Value = value;
        end
        parameters(i) = value;
        vectorEdit.Value = mat2str(reshape(parameters, 1, n));
    end

    function okButFcn()
        delete(fig);
    end

    function keyPressFcn(evt)
        disp(evt.Source);
        if strcmp(evt.Key, 'return')
            okButFcn();
        elseif strcmp(evt.Key, 'escape')
            resetValues();
        end
    end
```

- `updateAllValues`：根据输入的向量，更新所有的输入框，调用`editValueChangedFcn`。
- `resetValues`：调用`editValueChangedFcn`，重置所有的输入框为默认值。
- `editValueChangedFcn`：编辑框值变化回调函数，更新参数数组，更新向量编辑框。
- `okButFcn`：确定按钮回调函数，关闭窗口，对应`uiwait`。
- `keyPressFcn`：键盘事件回调函数，回车键关闭窗口，ESC键重置所有参数。

整个函数的代码可以在MathWorks的社区找到，[dialog to edit parameters](https://www.mathworks.com/matlabcentral/fileexchange/172054-dialog-to-edit-parameters).

或者在这里下载：

- [editParameters.m](/matlab-code/editParameters.m)
- [editParametersTest.m](/matlab-code/editParametersTest.m)

运行界面如图所示。

![在这里插入图片描述](/matlab-img/param-dialog-example.png)

### 测试
应该还可以增加一些测试，比如输入非法字符，输入非法向量等。但是这么个简单东西，真没必要。


## 参考
- [GUI系列](/tags/ui/)

## 总结

1. Matlab的GUI设计有两种方式，一种是App Designer，一种是编程设计。
2. `uifigure`创建窗口及配套控件和布局，构造的应用与App Designer类似。
3. 编写GUI的函数，可以通过调用规范来规范输入参数，确保输入参数正确。
4. 回调函数，因为有匿名函数调用的方式，可以很方便地在运行时绑定所需值，正确实现功能。
