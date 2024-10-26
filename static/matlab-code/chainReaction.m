function fig = chainReaction(m, n)
arguments
    m (1,1) {mustBePositive, mustBeInteger} = 10
    n (1,1) {mustBePositive, mustBeInteger} = 10
end


fig = uifigure(Visible='off', Name="Chain Reaction");
fig.Position(3:4) = [800, 600];
movegui(fig, 'center');

g = uigridlayout(fig, [m, n]);

buttons = cell(m*n, 1);

for i = 1:m
    for j = 1:n
        b = uibutton(g, ...
            'Text', sprintf('%d,%d', i, j), ...
            'BackgroundColor', 'white');
        buttons{(i-1)*n + j} = b;
        b.Layout.Row = i;
        b.Layout.Column = j;
    end
end

% buttons = buttons(randperm(numel(buttons)));

get(0,'screensize');
screenWidth = aref(get(0, 'ScreenSize'), 1, 4);

r = java.awt.Robot;

% setup chain reaction for coloring
for idx = 1:numel(buttons)
    b = buttons{idx};
    b.ButtonPushedFcn = {@chainReactionFcn, buttons, idx, r, fig,  screenWidth};
end

clear getNextColor;

fig.Visible = 'on';
end

function chainReactionFcn(src, ~, buttons, idx, robot, f, sw)

f.Name = ['Chain Reaction: '  src.Text];


src.BackgroundColor = getNextColor(f, numel(buttons));
pause(0.1);
% src.BackgroundColor = c;

nextIdx = mod(idx, numel(buttons)) + 1;
nb = buttons{nextIdx};

% feval(nextButton.ButtonPushedFcn{1}, nextButton, [], buttons, nextIdx);
% chainReactionFcn(nextButton, [], buttons, nextIdx);
pause(0.01)
p1 = f.Position(1:2) + nb.Position(1:2);
x = p1(1) + nb.Position(3) / 2;
y = sw - (p1(2) + nb.Position(4) / 2);

% move mouse to the center of the next button and click
pause(0.05)
robot.mouseMove(x, y);
pause(0.01)
robot.mousePress(java.awt.event.InputEvent.BUTTON1_MASK);
pause(0.01)
robot.mouseRelease(java.awt.event.InputEvent.BUTTON1_MASK);
end

function c = getNextColor(fobj, n)
persistent count
if isempty(count)
    count = min(max(2*n, 100), 256);
    fprintf('initlize count: %d\n', count);
end
persistent cm 
if isempty(cm)
    cm = colormap(fobj, sky(count));
    fprintf('Initialize cm: %d\n', size(cm, 1));
end

persistent idx
if isempty(idx)
    idx = 1;
    fprintf('Initialize idx = %d\n', idx);
end
fprintf('return idx = %d/%d\n', idx, count);
c = cm(idx, :);
idx = idx + 1;
if idx > count
    idx = 1;
end
end
