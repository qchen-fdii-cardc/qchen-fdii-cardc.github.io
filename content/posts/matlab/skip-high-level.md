+++
title = 'High_Level_Skip_in_Matlab中的高端跳过循环'
date = 2024-11-21T17:14:47+08:00
draft = false
mathjax = false
categories = ['matlab']
tags = ['matlab', 'tutorial', 'continue', 'gui']
toc = true
tocBorder = true
+++


## 循环控制

Matlab的循环控制语句有两个，分别是`for`和`while`。`for`循环是一种计数循环，`while`循环是一种条件循环。在循环中，有时候我们需要跳过一些循环，这时候就需要用到`continue`语句；当我们需要提前结束循环，这时候就需要用到`break`语句或者`return`语句。

### `for`循环

Matlab的`for`循环基本语法如下。

```matlab
for column = Columns
    % for each column of Columns, do the following
    % ...
    if needsToBreak
        break
    end

    if needsToReturn
        return
    end


    if needsToSkipFollowing
        contine
    end
end
```

在循环过程中，有三种控制循环的方式，其中`break`直接跳到`end`的外面；`return`（在函数里）直接跳出函数；`continue`则忽略接下来到`end`的所有代码，进入下一个列的循环。

值得注意的是，`for`循环被循环数组的列。`1:10`是一个行向量，所以`for i = 1:10`循环10次，`i`从1到10；如果`for c = C`中`C`是一个矩阵，那么就是循环`C`的每一个列。

```matlab
for i = magic(3), disp(i), end
     8
     3
     4

     1
     5
     9

     6
     7
     2
```

### `while`循环

这个循环语法则更为简单，也更加本质。

```matlab
while conditionExpression    
    if needsToBreak
        break
    end

    if needsToReturn
        return
    end

    if needsToSkipFollowing
        contine
    end
end
```

首先检测表达式`conditionExpression`是否为真，否则跳到对应的`end`之后。在这个循环体重，同样可以采用`break`/`return`/`contine`进行中断和跳过的操作。



这两种低级的操作太没有水准，今天我们必须高一点高端的跳过循环的操作。

## 高端的跳过之一

这个实现这样的功能，后台计算过程中，有一个进度条，提示计算的当前步骤，提供一个按钮来发送跳过当前步骤的提示，然后的命令窗口进行确认。

![](/matlab-img/waitbar.png)

这个界面上有一个按钮，这个按钮的文字我还没找到办法修改，按照道理，应该可以找到一个Childeren，改掉那啥...

每次点击取消，就可以把控制权切换到命令行窗口，确认跳过，继续运行。
![](/matlab-img/confirmation.png)


这个代码里面有几个有意思的函数：

- `waitbar`，产生一个等待进度条窗口，是传统基于Figure的界面的一部分
- `setappdata`/`getappdata`，把数据存储在图形对象中，用一个字符串作为索引，邪路
- `commandwindow`把焦点放回命令行窗口，是个很实用的函数
- `gcbf`返回回调函数调用的对象，比如`delete(gcbf)`删掉回调函数对应的图形对象
- `pause`，暂停运行若干时间，函数是一个浮点数，则为暂停的秒数

这里就是一个典型的`while`循环用`continue`跳过部分代码继续运行。从这里也可以看到，恰面的`break`代码的判断部分，很容易就在`while`的表达式中隐含。所以，`break`在`for`循环中是刚需，在`while`中有可能简化合并。


- [源代码](/matlab-code/skipExample.m)
```matlab
function skipExample

cleanObj = onCleanup(@cleanAll);

hWaitbar = waitbar(0, 'Iteration 1', 'Name', 'Solving problem', ...
    'CreateCancelBtn', @(~,~)fcn);

hWaitbar.CloseRequestFcn = @(~,~)cleanAll;

setappdata(hWaitbar, 'skip', false);

n = 3;
i = 0;
try
    while(ishandle(hWaitbar))
        if getappdata(hWaitbar, 'skip')
            setappdata(hWaitbar, 'skip', false);
            % for keyboard conformation to continue
            fprintf("%d   - skip this round, ", i)
            fprintf("Press any key to continue...")
            commandwindow; % switch to command window
            pause % delete to just run through
            fprintf("\n");
            continue;
        end
        % pretent to calculate sth
        pause(0.5)
        
        i = i + 1;
        waitbar( mod(i, n) / (n-1),  hWaitbar,  ['Iteration ' num2str(i)]);
    end
catch ME
    fprintf("%s: %s\n", ME.identifier, ME.message);
    cleanAll;
end
end

function cleanAll
delete(gcbf);
end

function fcn
setappdata(gcbf, 'skip', true);
end
```
这里的整个都很丑，还会因为这样那样的情况无法暂停导致窗口关不掉，用`close all`也不行，因为，`handleVisibility='off'`。这个时候就需要下面这个大杀器，干掉所有窗口，一旦你走上用Matlab编GUI程序的邪路，就会发现下面这两个语句的魅力！

```matlab
set(groot,'ShowHiddenHandles','on')
delete(get(groot,'Children'))
```

## 太丑不能忍之后的考虑

因为要用同一个按钮（右上角$\times$）来完成两个职责：跳过和退出，造成了上面那个例子所有的混乱，窗口的关闭和句柄删除会干扰程序的逻辑。这就告诉我们，在设计GUI是一定要好好分析业务逻辑，一个界面元素（一组界面元素）应该有内聚性很高的职责，也就是只做一件事情。

出于这个考虑，我们又设计了下面的例子，这里，取消按钮和右上角的关闭按钮，都按照原有的语义，负责关闭进度条，取消/停止计算任务。那么跳过怎么办呢？我们不能用Ctrl-C来完成，因为这个快捷键有很高的优先级，会中止正在进行的计算。

那么我们设置一个快捷键，Ctrl-K来跳过一步计算。这里的关键就是给进度条设置监控键盘事件的回调函数。

```matlab
f.KeyReleaseFcn = @skipABeat;

function skipABeat(src, evt)
    if ismember('control', evt.Modifier) && evt.Key == 'k'
        setappdata(src, 'skip', true);
    end
end
```

基本上，基于Figure或者基于UiFigure的GUI程序的回调函数都至少包含两个基本的参数，一个是发出消息的来源，一个是事件对象。对于键盘事件就是一个`KeyData`数据结构：

```matlab
KeyData - 属性:

    Character: 'k'
     Modifier: {1x0 cell}
          Key: 'k'
       Source: [1x1 Figure]
    EventName: 'KeyRelease'
```
上面这个对象，很容易得到，我们只需要运行这个，然后把焦点放在图窗上，按动键盘就可以得到实际拿到的事件对象是什么样子的。这是GUI编程中一个很重要的思路：因为系统可以运行，所以想要什么信息就要构造程序来获得。

```matlab
f = figure;
f.KeyReleaseFcn = @(src, evt)disp(evt)
```

大概就是这样的，所以我们可以判断是否按下了`k`，是否同时按着`control`。

这个GUI就让人满意很多，因为概念上更加清晰，就不需要打那么多补丁。

- [源代码](/matlab-code/skipExample3.m)
```matlab
function skipExample3


f = waitbar(0,'1','Name','Approximating pi...',...
    'CreateCancelBtn','setappdata(gcbf,''canceling'',1)', ...
    'Position', [0, 0, 480, 360]);


movegui(f, 'center');

f.KeyReleaseFcn = @skipABeat;

setappdata(f,'canceling',0);

% Approximate pi^2/8 as: 1 + 1/9 + 1/25 + 1/49 + ...
pisqover8 = 1;
denom = 3;
valueofpi = sqrt(8 * pisqover8);

steps = 20000;
for step = 1:steps
    % Check for clicked Cancel button
    if getappdata(f,'canceling')
        break
    end

    if getappdata(f, 'skip')
        fprintf("Skip step = %d\n", step);
        setappdata(f, 'skip', false);
        continue
    end
    
    % Update waitbar and message
    waitbar(step/steps,f,sprintf('%12.9f',valueofpi))
    
    % Calculate next estimate 
    pisqover8 = pisqover8 + 1 / (denom * denom);
    denom = denom + 2;
    valueofpi = sqrt(8 * pisqover8);
end

delete(f)
end


function skipABeat(src, evt)
    if ismember('control', evt.Modifier) && evt.Key == 'k'
        setappdata(src, 'skip', true);
    end
end
```

## 总结

1. 两种循环方式，`for`和`while`
2. `break`/`return`/`continue`控制循环提前结束和跳过
3. `for`循环中使用`break`更加自然
4. `while`循环中使用`break`的条件可以合并到`while`表达式中
5. 设计UI的过程中，一定要考虑清楚每个元素的职责，不要让一个元素负责多个职责