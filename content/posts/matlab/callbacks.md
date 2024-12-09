+++
title = 'Callbacks_in_MATLAB中APP界面回调函数'
date = 2024-10-21T17:20:23+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'gui', 'callback', 'uidropdown']
toc = true
tocBorder = true
+++


## 回调函数

### 用法
回调函数，是Matlab GUI和App设计中的一个核心的内容。通过用户注册回调函数，在界面操作时App代码会自动调用相应代码，实现各种功能。编程实现GUI App时，回调函数的注册方式有两种：

```matlab
function fig = createApp
fig = uifigure;

btn = uibutton(fig, 'push', 'ButtonPushedFcn', @buttonPushed);
% btn = uibutton(fig, 'push', ButtonPushedFcn=@buttonPushed);

btn.ButtonPushedFcn = @buttonPushed;

% 或者
fh = @(src, event) disp('Button pushed');
btn.ButtonPushedFcn = fh;

end
```

具体回调函数的形式请查看帮助，一般而言，函数有两个参数，第一个参数是发起回调的对象，这里就是`btn`，第二个参数是事件数据，这里是`ButtonPushedData`。

回调函数设定时的值可以有三种形式：

- 函数句柄，如`@buttonPushed`
- 函数句柄和参数，如`{@buttonPushed, arg1, arg2}`
- 匿名函数，如`@(src, event) disp('Button pushed')`

### 全局函数、局部函数和嵌套函数

以这个函数为例：

```matlab
function buttonPushed(src, event)
    disp('Button pushed');
end
```

函数可以通过三种方式来实现：

- 函数文件形式的全局函数，单独有一个`buttonPushed.m`文件
- 放在文件最后的局部函数，在这里就是函数文件`function ...... end`之后的局部函数
- 放在函数内部的嵌套函数

嵌套函数的定义形式如下：

```matlab
function fig = createApp

    fig = uifigure;
    btn = uibutton(fig, 'push', 'ButtonPushedFcn', @buttonPushed);
    
    function buttonPushed(src, event)
        disp('Button pushed');
    end
end
```

这三种情况没有本质的区别，其中，嵌套函数可以共享在函数调用之前定义的所有变量，这是一个很有用的特性。

全局函数和局部函数都不能共享变量，但是可以通过`global`关键字来共享变量。主要通过`{@functionName, arg1, arg2}`的形式来传递参数。另外，匿名函数也捕获函数，比如：

```matlab
@(src, event) funcWithExtraArgs(args1, src, event, arg2)
```

这种情况下，`funcWithExtraArgs`函数会被调用，参数是`args1, src, event, arg2`。这个时候，整个函数的排列方式是按照用户的需要自行定义的。


### 内部实现

用于App响应用户的输入。在[类层次结构](#ui相关类)中给出的UI相关的类结构中，实现了一个对用户提供的响应函数的支持。

- `matlab.ui.control.internal.controller.ComponentController`
- `appdesservices.internal.interfaces.model.AbstractModel`

其中两个重要的类是`ComponentController`和`AbstractModel`，这两个类是`handle`的子类，构成一个`MVC`模式。`ComponentController`是控制器，`AbstractModel`是模型。

在`ComponentController`中， 有一个函数：

```matlab
function handleUserInteraction(obj, clientEventName, eventData, callbackInfo)
    % Method to be called by the subclasses when handling a user
    % interaction that results in either:
    % - a user callback executing  (e.g. ButtonPushedFcn)
    % - a property update and a user callback executing (e.g.
    % ValueChangedFcn)
    % - any number greater than 2 of the above (e.g. 'mouseclicked'
    % results in 2 callbacks)
    %
    % Typically, the subclasses would implement handleEvent, and in
    % the case of a user interaction, call handleUserInteraction.
    %
    % INPUTS:
    %
    %  - clientEventName:  event name of the client side event
    %                       that this is a response to
    %
    %  - eventData: event data of the client side event that this
    %               is a response to
    %
    %  - as many cells as the number of callbacks to execute.
    %  Minimum is 1.
    %  See executeUserCallback for the formatting of each cell.
    %
    % Example:
    %
    % obj.handleUserInteraction(...
    %       'mousedragging', ...
    %       event.Data, ...
    %       {'ValueChanging', eventData}, ...
    %       {'ValueChanged', eventData, 'Value', newValue}, ...
    %       );
    
    assert(nargin == 4);           
    
    obj.Model.executeUserCallback(callbackInfo{:});

    if(~isvalid(obj))
        % It is possible the user callback deleted the component
        %
        % If so, don't do any more
        return;
    end
    % Force the view to process the value update before
    % emitting the event.
    % If the property is revered to its old value in a callback
    % (its own or from another component),
    % the visual might not update because of the peer node
    % coalescing events from property sets.
    % Ensure that the visual will react to a potential
    % reversion by forcing the view to process the current
    % value.
    %
    % We don't need to specify any property names in
    % 'refreshProperties' because simply sending an event
    % flushes the propertiesSet event queue. If we explicitly
    % passed in the propertyName, the view would refresh
    % twice in the case of the reversion from the callback of
    % another component.
    %
    % see g1124873 and g1218934
    
    obj.refreshProperties({});


    % After all matlab events for this client side event have been
    % emitted and callbacks processed, send an event to the client
    % if the event is registered to use an event coalescing
    % mechanism.
    % Need to check if obj and ViewModel are valid or not because
    % the user's callback could delete the app or the component
    % see g1336677
    coalescedEventIsField = isfield(eventData, 'CoalescedEvent');
    if(isvalid(obj) && coalescedEventIsField && eventData.CoalescedEvent)
        obj.sendFlushEventToClient(obj.Model, clientEventName);
    end
    
end
```

这里面的`obj.Model`就是一个`AbstractModel`的对象

```matlab
function executeUserCallback(obj, matlabEventName, matlabEventData, propertyName, propertyValue)
    % Execute user callbacks associated with 'matlabEventName'.
    % If a property-value pair is also provided, the property will
    % be updated before the callbacks are executed.
    %
    % INPUTS:
    %
    %  - MatlabEventName:  string representing the event that the component
    %                model should emit as a result of the user interaction
    %  - MatlabEventData:  eventdata associated with eventName
    %
    % Example: obj.executeUserCallback('ButtonPushed', 'ButtonPushed', eventData);
    %
    % Optional INPUTS:
    %
    %  - propertyName:    name of the property to be modified as
    %                     a result of the user interaction if any
    %  - propertyValue:   value to update the property to
    %
    % Example: obj.executeUserCallback('ValueChanged', eventData, 'Value', newValue);                        
    
    assert(nargin == 3 || nargin == 5);
    
    if(nargin == 3)
        % There is no property to update, just emit the event
        
        % Have the model emit the event
        % The event handling system will execute the callbacks
        % associated with this event.
        notify(obj, matlabEventName, matlabEventData);
        
    else
        % propertyName and propertyValue were passed in as inputs
        % The property needs to be updated before sending the event
        
        oldValue = obj.(propertyName);
        
        % Check that the property value has indeed changed
        if(isequal(oldValue,propertyValue))
            % The value has not changed, do not emit event.
            % This check is a catch all for instances where the
            % view does send an event even when the value didn't
            % really change.
            return;
        end
        
        % Update the property value
        obj.(propertyName) = propertyValue;                                

        % Mark properties dirty
        %
        % Usually, the property is private, such as 'PrivateFoo'
        obj.markPropertiesDirty({propertyName});                 
        
        % Have the model emit the event
        % The event handling system will execute the callbacks
        % associated with this event.
        notify(obj, matlabEventName, matlabEventData);
    end
end        
```

这些代码是Matlab的UI组件的回调函数的核心代码，通过这些代码，可以实现UI组件的回调函数的功能。


## 例子

下面我们改造一下官方帮助文件中的例子，搞一个用下拉列表来更换背景颜色的例子。运行的效果如下：

![app](/matlab-img/dropdown.png)

文件下载地址：

- [updateDropDown函数文件](/matlab-code/updateDropDown.m)
- [Icon图形文件](/matlab-img/icon.jpg)

```matlab
function fig = updateDropDown
fig = uifigure('Position', [100 100 480 300], 'Visible', 'off', Name='DropDown Example');
fig.Icon = 'icon.jpg';
movegui(fig, 'center');

fg = uigridlayout(fig, [3 2]);
fg.ColumnWidth = {'fit', '1x'};
fg.RowHeight = {'1x', 'fit', '1x'};


% color tareget
p = uipanel(fg);

% 'red', 'green', 'blue', 'cyan', 'magenta', 'yellow', 'black', 'white' 和 'none'
% 有效的十六进制颜色代码由 '#' 后跟三个或六个十六进制数字组成
p.BackgroundColor = 'white';

%  'none' | 'etchedin' | 'etchedout' | 'beveledin' | 'beveledout' | 'line'。
p.BorderType = 'line';

p.Title = 'Color Target';
p.Layout.Row = [1, 3];
p.Layout.Column = 2;
% add text in the center of the panel
g = uigridlayout(p, [1 1]);
g.RowHeight = {'1x'};
g.ColumnWidth = {'1x'};
t = uilabel(g, 'Text', '黑色文字',...
    'FontSize', 48, ...
    'HorizontalAlignment', 'center',...
    'VerticalAlignment', 'center');

% no height resize

t.Tooltip = "'red', 'green', 'blue', 'cyan', 'magenta', 'yellow', 'black', 'white'; 有效的十六进制颜色代码由 '#' 后跟三个或六个十六进制数字组成";


dd = uidropdown(fg, ...
    'Editable', 'on', ...
    'ValueChangedFcn', {@addItems, t});

dd.Tooltip = "选择或者编辑颜色";
dd.Layout.Row = 2;
dd.Layout.Column = 1;


% dd.Items = ["red", "yellow", "green"];
dd.Items = {'red','green','blue','cyan','magenta','yellow','black','white' };
for idx = 1:numel(dd.Items)
    setUpBg(dd, idx, dd.Items{idx});
end

fig.UserData = struct('dropdown', dd,...
    'panel', p, ...
    'text', t);

t.BackgroundColor = dd.Value;

fig.Visible = 'on';

end

function setUpBg(target, idx, c)
% table, tree, list box, or drop-down UI component
st = uistyle("BackgroundColor", c);
addStyle(target, st, "item", idx);
end


function addItems(src, event, p)
arguments
    src (1,1) matlab.ui.control.DropDown
    event (1,1) matlab.ui.eventdata.ValueChangedData
    p (1,1) matlab.ui.control.Label
end
val = src.Value;

if event.Edited
    src.Items{end+1} = val;
    setUpBg(src, numel(src.Items), val);
end

p.BackgroundColor = val;

end
```

## UI相关类

App Designer中的UI编程的函数和类在`%MATLAB_ROOT%\toolbox\matlab\uicomponents\uicomponents`目录下，根据安装位置，`%MATLAB_ROOT%`可能是`C:\Program Files\MATLAB\R2023b`。在`+matlab.ui`目录下有很多UI相关的类，这些类是App Designer的基础类，可以通过这些类来实现App Designer中的功能。

如果把这里面大部分类的继承关系整一个图，大概是这样的：

![](/matlab-img/matlab-ui-classes.png)

这个图什么都看不清，但是实际上，运行下面的代码，这个图可随便放大，也能通过鼠标在图上查看节点信息（名称、入度、出度），可以很容易地看到继承关系。

```matlab
paths = walkToTop({'matlab.ui.Figure', ...
    'matlab.ui.container.Tree', ...
    'matlab.ui.container.TreeNode', ...
    'matlab.ui.container.GridLayout', ...
    'matlab.ui.container.CheckBoxTree', ...
    'matlab.ui.container.TabGroup', ...
    'matlab.ui.container.Tab', ...
    'matlab.ui.container.Panel',...
    'matlab.ui.container.ContextMenu', ...
    'matlab.ui.container.Menu', ...
    'matlab.ui.control.Table', ...
    'matlab.ui.control.AngularGauge', ...
    'matlab.ui.control.Button', ...
    'matlab.ui.control.CheckBox', ...
    'matlab.ui.control.DatePicker', ...
    'matlab.ui.control.DiscreteKnob', ...
    'matlab.ui.control.DropDown', ...
    'matlab.ui.control.EditField', ...
    'matlab.ui.control.Gauge', ...
    'matlab.ui.control.HTML', ...
    'matlab.ui.control.Hyperlink', ...
    'matlab.ui.control.Image', ...
    'matlab.ui.control.Knob', ...
    'matlab.ui.control.Label', ...
    'matlab.ui.control.Lamp', ...
    'matlab.ui.control.LinearGauge', ...
    'matlab.ui.control.ListBox', ...
    'matlab.ui.control.NinetyDegreeGauge', ...
    'matlab.ui.control.NumericEditField', ...
    'matlab.ui.control.RadioButton', ...
    'matlab.ui.control.RangeSlider', ...
    'matlab.ui.control.RockerSwitch', ...
    'matlab.ui.control.SemicircularGauge', ...
    'matlab.ui.control.Slider', ...
    'matlab.ui.control.Spinner', ...
    'matlab.ui.control.StateButton', ...
    'matlab.ui.control.Switch', ...
    'matlab.ui.control.TextArea', ...
    'matlab.ui.control.ToggleButton', ...
    'matlab.ui.control.ToggleSwitch',...
    'matlab.ui.eventdata.ButtonPushedData', ...
    'matlab.ui.eventdata.CheckedNodesChangedData', ...
    'matlab.ui.eventdata.ClickedData', ...
    'matlab.ui.eventdata.CollapsedChangedData', ...
    'matlab.ui.eventdata.ComponentInteraction', ...
    'matlab.ui.eventdata.ContextMenuOpeningData', ...
    'matlab.ui.eventdata.DataChangedData', ...
    'matlab.ui.eventdata.DoubleClickedData', ...
    'matlab.ui.eventdata.DropDownInteraction', ...
    'matlab.ui.eventdata.DropDownOpeningData', ...
    'matlab.ui.eventdata.FigureInteraction', ...
    'matlab.ui.eventdata.HTMLEventReceivedData', ...
    'matlab.ui.eventdata.HyperlinkClickedData', ...
    'matlab.ui.eventdata.ImageClickedData', ...
    'matlab.ui.eventdata.ListBoxInteraction', ...
    'matlab.ui.eventdata.MenuSelectedData', ...
    'matlab.ui.eventdata.MutualExclusiveComponentSelectionChangeData', ...
    'matlab.ui.eventdata.NodeCollapsedData', ...
    'matlab.ui.eventdata.NodeExpandedData', ...
    'matlab.ui.eventdata.NodeTextChangedData', ...
    'matlab.ui.eventdata.PasswordEnteredData', ...
    'matlab.ui.eventdata.SelectedNodesChangedData', ...
    'matlab.ui.eventdata.TableInteraction', ...
    'matlab.ui.eventdata.TreeInteraction', ...
    'matlab.ui.eventdata.ValueChangedData', ...
    'matlab.ui.eventdata.ValueChangingData'});


% get all names into a set
names = {};
for i = 1:length(paths)
    for j = 1:length(paths{i})
        if ~ismember(paths{i}{j}, names)
            names = [names, paths{i}{j}];
        end
    end
end


% initialize the digraph

dgObj = digraph;
for i = length(names):-1:1
    dgObj = addnode(dgObj, names{i});
end


for i = 1:length(paths)
    for j = 1:length(paths{i})-1
        dgObj = addedge(dgObj, paths{i}{j+1}, paths{i}{j});
    end
end

% visualize the digraph
plot(dgObj, 'Layout', 'layered', 'NodeLabel', dgObj.Nodes.Name, 'NodeColor', 'r', 'EdgeColor', 'b');
exportgraphics(gcf, 'matlab-ui-classes.png');

% find the roots and write markdown for me
parents = dgObj.Nodes{indegree(dgObj) == 0, :};
for i = 1:numel(parents)
    fprintf("- `%s`\n", parents{i});
end
```

最后这段话找出所有的基类（也就是`digraph`中入度为`0`的节点），并且把下面的Markdown帮我写好。其实，最上面那段`UI`对象的列表，我也是通过这段代码生成的……可以留做一个作业！


Matlab App开发的相关类的基类基本上是下面这些：

- `matlab.ui.eventdata.FigureInteraction`
- `matlab.ui.eventdata.internal.Interaction`
- `matlab.graphics.mixin.internal.GraphicsDataTypeContainer`
- `matlab.mixin.internal.CompactDisplay`
- `matlab.mixin.CustomDisplay`
- `matlab.mixin.Heterogeneous`
- `handle`

其中，`handle`是Matlab中的句柄类，所有的UI组件都是`handle`的子类，这个类的对象都是传递引用的，而不是传递值的。这在UI组件的回调函数中是非常有用的，可以通过传递UI组件的句柄，来实现UI组件之间的交互。


## 参考

1. [GUI系列](/tags/ui/)
2. [Create Callbacks for Apps Created Programmatically](https://ww2.mathworks.cn/help/matlab/creating_guis/write-callbacks-for-apps-created-programmatically.html)

## 总结

1. 回调函数是Matlab GUI和App设计中的一个核心的内容，通过用户注册回调函数，在界面操作时App代码会自动调用相应代码，实现各种功能。
2. 回调函数的注册方式有两种：函数句柄和匿名函数。
3. 回调函数具体实现方式有三种：全局函数、局部函数和嵌套函数。