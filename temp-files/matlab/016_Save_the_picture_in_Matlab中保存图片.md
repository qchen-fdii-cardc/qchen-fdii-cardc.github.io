## 图片文件

Matlab核心功能包括出图，印刷质量的图片输出是Matlab核心竞争力之一，matplotlib疯狂追赶，但还是差距明显。出图的含义就是：打印或者导出图形窗体的内容，可供后续使用。在Matlab中，这个行为被定义为打印和保存。

相关的函数有三类：

- 导出
    - `exportgraphics` 导出绘图和图形内容（从R2020a开始）
    - `copygraphics` 复制图形内容到剪贴板(从R2020a开始)
    - `exportapp` 导出App Designer应用程序的内容(从R2020b开始)
    - `getframe` 捕获坐标区或图形窗口的内容作为帧
    - `saveas` 保存图形为特定文件格式
    - `hgexport` 导出图形窗口
- 打印
    - `print` 打印图形
    - `orient` 设置打印时的方向（横向和纵向）
- 保存
    - `openfig` 打开.fig文件
    - `savefig` 保存图片


## 保存

包括其实是一个很好的起点，当我们幸幸苦苦绘制一幅图片，精心调节各种参数，最后得到一个满意的结果，当然希望能够在未来某个时间拿来重用。这个时候，`openfig`和`savefig`就很好解决问题。这个函数保存图形窗体完整信息，后续可以通过`openfig`打开，继续编辑。

```matlab
% 绘图
figure;
peaks;


% 保存图形窗体
savefig(h, 'peaks.fig');
% 或者当前图形窗体
savefig('peaks.fig');

```

![在这里插入图片描述](https://i-blog.csdnimg.cn/direct/769d6717ef334b5e84ac5df9953fa7f7.png#pic_center)


`savefig`有一个额外的参数，就是`'compact'`，这个参数可以减小文件大小，但是仅限于R2014b及以后的版本，能把文件刚过150kb减小到100kb出头。

打开保存的文件，用`openfig`函数，就可以继续编辑。函数提供是否新建窗体、是否显示窗体的选项。类似于对窗体通过`set`函数设置`'Visible'`属性来控制可见性，对于不可见的窗体，依然可以通过`savefig`保存。

```matlab
% 打开图形窗体
h = openfig('peaks.fig', 'new', 'invisible');

% 编辑
title('peaks');
xlabel('x');
ylabel('y');

% 保存
savefig(h, 'peaks_new.fig');

% 显示
set(h, 'Visible', 'on');
```

### `uicontrol`保存

在使用上面这对函数时，界面上的`uicontrol`不会丢，其回调函数也会被保存，因为`uicontrol`的回调是普通的字符串！这就是为什么GUIDE和基于Figure的App那么渣的原因，因为它们的回调函数是字符串，不是函数句柄，不方便写，不方便用。

```matlab
h = figure;
peaks;

button = uicontrol('Style', 'pushbutton', 'String', 'Save', ...
    'Position', [20 20 50 20], 'Callback', 'disp(''Save'')');

savefig(h, 'peaks.fig');

h2 = openfig('peaks.fig', 'new', 'visible');
```
两个窗体的`uicontrol`都会被保存，`button`的回调函数也会被保存。


### 推荐做法

推荐对于每个自己辛苦画的图片，应该保存一个`.fig`文件，这样可以在未来继续编辑，而不是重新画一遍，有时候，Matlab的绘图脚本会被弄到面目全非，这时候，打开`.fig`文件就能救你狗命，别问我为什么知道。


```
current = string(datetime("now",'Format', "yyyyMMddHHmmss"));   % 保存当前时间
mkdir(current);                                                 % 创建文件夹
figs = findall(0, 'type', 'figure');                            % 获取所有图形窗体，这里的0就是根窗体，也可以用`groot`来引用
fn = @(x)  [x.Name, num2str(x.Number), '.fig'];                 % 生成文件名
arrayfun(@(x) savefig(x, fn(x)), figs);                         % 保存所有图形窗体
arrayfun(@(x), movefile(fn(x), current), figs);                 % 移动文件到备份文件夹
```

## 打印

打印是另一个保存的方式，不过是保存到纸上。`print`函数可以打印图形，也可以保存图形到文件。`print`函数的参数很多，可以设置很多打印选项，比如打印机、纸张大小、方向、分辨率等等。

```matlab
% 打印
print -dpng peaks.png;
print -dpdf peaks.pdf;
print -depsc peaks.eps;
print -djpeg peaks.jpg;
print -dtiff peaks.tiff;
```

`print`函数的参数`-d`后面跟的是文件格式，可以是`png`、`pdf`、`eps`、`jpeg`、`tiff`等等，这些都是Matlab支持的文件格式。

`print`函数还可以设置很多参数，比如`-r`设置分辨率，`-fillpage`填充整个页面，`-bestfit`最佳适应页面，`-loose`松散，`-tight`紧凑，`-append`追加到文件末尾等等。

还可以打印到剪贴板，这样就可以直接粘贴到Word、PPT等软件中。

## 导出

用`exportgraphics`函数可以导出图形，这个函数是从R2020a开始引入的，可以导出图形到很多格式，比如`png`、`pdf`、`eps`、`jpeg`、`tiff`等等。这个导出将不包括图形窗体中的UI 组件，只含括图形对象。

```matlab
% 导出
exportgraphics(gcf, 'peaks.png');
exportgraphics(gcf, 'peaks.pdf');
```

还可以通过`Resolution`参数设置分辨率，`BackgroundColor`设置背景颜色（`None`为透明背景），`ContentType`设置内容类型等等。

当输出是`pdf`时，可以设置`ContentType`为`vector`，这样输出的`pdf`文件是矢量图，可以无限放大，不会失真。此外还可以通过`Append`参数设置是否追加到文件末尾，构成多页`pdf`文件。

```matlab
% 导出
exportgraphics(gcf, 'peaks.pdf', 'ContentType', 'vector');

% 追加
exportgraphics(gcf, 'peaks.pdf', 'ContentType', 'vector', 'Append', true);
```

这个函数所针对的图形对象，可以是

- `figure`对象，`uifigure`对象
- `axes`对象,Axes对象、PolarAxes对象、GeographicAxes对象
- 独立的可视化对象，比如`heatmap`
- 分块图布局，`tiledlayout`对象
- 图中的容器对象：Panel, Tab, ButtonGroup

对于`uicontrol`和`uixxx`那些空间，`exportgraphics`函数都不支持。可以使用`exportapp`函数导出App Designer应用程序的内容。

```matlab
function savapp
f = uifigure;
ax = uiaxes(f,'Position',[25 25 400 375]);
plot(ax,[0 0.3 0.1 0.6 0.4 1])
b = uibutton(f,'Position',[435 200 90 30],'Text','Save Plot');
b.ButtonPushedFcn = @buttoncallback;

    function buttoncallback(~,~)
        filter = {'*.jpg';'*.png';'*.tif';'*.pdf';'*.eps'};
        [filename,filepath] = uiputfile(filter);
        if ischar(filename)
            exportgraphics(f,[filepath filename]);
        end
    end


b2 = uibutton(f,'Position',[435 250 90 30],'Text','Save APP Plot');
b2.ButtonPushedFcn = @buttoncallback2;

    function buttoncallback2(~,~)
        filter = {'*.jpg';'*.png';'*.tif';'*.pdf';'*.eps'};
        [filename,filepath] = uiputfile(filter);
        if ischar(filename)
            exportapp(f,[filepath filename]);
        end
    end
end
```

![在这里插入图片描述](https://i-blog.csdnimg.cn/direct/fb96265c055b48d38b4d17c713797ae5.jpeg#pic_center)
![在这里插入图片描述](https://i-blog.csdnimg.cn/direct/917e3373c0504d1f9d7b5095ee624f81.jpeg#pic_center)


如果能够使用新版本的Matlab，推荐使用`exportgraphics`函数，这个函数导出的图片周围的空白会自动裁剪。

## `getframe`和`imwrite`

这两个函数过于高级，应该只有在需要导出到特殊格式，比如`bmp`，或者需要构成动画的才去使用。`getframe`函数可以捕获坐标区或图形窗口的内容作为帧，`imwrite`函数可以将帧保存为图像文件。

```matlab
% 获取帧
f = figure;
peaks;
frame = getframe(f);

% 保存
imwrite(frame.cdata, 'peaks.bmp');
```


## 总结比较

- `savefig`保存图形窗体，可以继续编辑
- `print`打印图形，也可以保存图形到文件
- `exportgraphics`导出图形，不包括图形窗体中的UI组件
- `getframe`和`imwrite`可以保存帧为图像文件
- `exportapp`导出App Designer应用程序的内容
