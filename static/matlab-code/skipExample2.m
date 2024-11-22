function fig = skipExample2

fig = uifigure();
fig.UserData = struct('skip', false);



calculated = [];
skipped = [];
i = 0;

hWaitbar = uiprogressdlg(fig,'Title', 'Solving problem', ...
    'Indeterminate', 'on', ...
    'Cancelable', 'on',...
    'CancelText', '取消');

hWaitbar.KeyReleaseFcn = @skipABeat;

while (true)
    i = i + 1;
    if hWaitbar.CancelRequested
        break
    end
    if fig.UserData.skip
        fprintf("Skip %d ...\n", i)
        fig.UserData.skip = false;
        continue
    end

    % pretent to calculate sth
    pause(0.5 * rand)    
    hWaitbar.Message = sprintf("Calculation %d", i);

    calculated = [calculated(:); i];
end
close(hWaitbar);

g = uigridlayout(fig, [1,1], 'ColumnWidth', {'1x'}, 'RowHeight', {'1x'});

Tasks = sort([calculated(:); skipped(:)]);
Skipped = arrayfun(@(i)ismember(i, skipped), Tasks);
Finished = arrayfun(@(i)ismember(i, calculated), Tasks);

data = table(Tasks, Skipped, Finished);

t = uitable(g, "Data", data);

t.Layout.Row = 1;
t.Layout.Column = 1;
end



function skipABeat(src, evt)
disp(src)
disp(evt)
    if ismember('control', evt.Modifier) && evt.Key == 'k'
       f = gcbf;
       f.UserData.skip = true;
    end
end