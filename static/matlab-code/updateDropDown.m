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
    'FontSize', 48, ...'
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
