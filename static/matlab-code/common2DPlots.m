function f = common2DPlots
f = uifigure(Name='Common 2D Plots', Visible='off', WindowState='minimized');
movegui(f, 'center');
g = uigridlayout(f, [1, 1]);
ax = uiaxes(g, Visible='off');
ax.Layout.Row = 1;
ax.Layout.Column = 1;

menu = uimenu(f, Text="Demos");
tb = uitoolbar(f);

texts = {...
    "Line Plots", ...
    "Multiple Lines Plots", ...
    "Bar Plots", ...
    "Stairstep Plots", ...
    "Errorbar Plots", ...
    "Stem Plots", ...
    "Scatter Plots", ...
    "Scatter Colorbar Plots", ...
    };
fcns = {...
    @linePlotFcn, ...
    @multipleLinePlotFcn, ...
    @barPlot,...
    @stairStepPlot, ...
    @errorBarPlot, ...
    @stemPlot, ...
    @scatterPlot, ...
    @scatterColorPlot, ...
    };
n = numel(texts);

for idx = 1:n
    fn = matlab.lang.makeValidName(texts{idx}) + ".png";
    
    if ~exist(fn, 'file')
        feval(fcns{idx}, ax);
        exportgraphics(ax, fn, Resolution=10);
    end
    
    cb = makeCallback(fcns{idx}, ax, texts{idx});
    
    uimenu(menu, Text=texts{idx}, ...
        MenuSelectedFcn=cb);
    uipushtool(tb, Tooltip=texts{idx}, ...
        Icon=fn, ...
        ClickedCallback=cb)
end

uimenu(menu, Text="Quit", ...
    Accelerator="Q", ...
    Separator='on', ...
    MenuSelectedFcn=@(~, ~)close(f));


clearAll(ax);

f.WindowState = "normal";
f.Visible = 'on';

end

function fh = makeCallback(func, ax_handle, textLabel)
    function retCb(~,  ~)
        clearAll(ax_handle);
        feval(func, ax_handle);
        ax_handle.Title.String = textLabel;
        ax_handle.Title.FontWeight = 'bold';
        ax_handle.Title.FontSize = 24;
        ax_handle.Visible = 'on';
    end
fh = @retCb;
end


function clearAll(ax_handle)
colorbar(ax_handle, 'off');
cla(ax_handle);
end

function scatterPlot(ax_handle)
load patients Height Weight Systolic
scatter(ax_handle, Height,Weight)
xlabel(ax_handle,'Height')
ylabel(ax_handle,'Weight')
end

function scatterColorPlot(ax_handle)
load patients Height Weight Systolic
scatter(ax_handle, Height,Weight, 20,Systolic)
xlabel(ax_handle,'Height')
ylabel(ax_handle,'Weight')
colorbar(ax_handle);
end


function stemPlot(ax_handle)
x = 0:0.1:4;
y = sin(x.^2) .* exp(-x);
stem(ax_handle, x, y);
end

function polarPlot(ax_handle)
clearAll(ax_handle);
theta = 0:0.01:2*pi;
rho = abs(sin(2*theta) .* cos(2*theta));
polarplot(ax_handle, theta, rho);
end

function errorBarPlot(ax_handle)
x = -2:0.1:2;
y = erf(x);
eb = rand(size(x)) / 7;
errorbar(ax_handle, x, y, eb);
end


function stairStepPlot(ax_handle)
x = 0:0.25:10;
y = sin(x);
stairs(ax_handle, x, y);
end

function barPlot(ax_handle)
x = -2.9:0.2:2.9;
y = exp(-x .* x);
bar(ax_handle, x, y);
end


function linePlotFcn(ax_handle)
x = 0:0.05:5;
y = sin(x.^2);
plot(ax_handle, x, y);
end

function multipleLinePlotFcn(ax_handle)
x = 0:0.05:5;
y1 = sin(x.^2);
y2 = cos(x.^2);
plot(ax_handle, x, y1, x, y2);
end