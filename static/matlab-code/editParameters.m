function parameters = editParameters(name, labels, defaultValues, units)
%% editParameters
% Edit parameters in a dialog.
%
% Parameters:
% - name: name of the dialog
% - labels: cell array of labels
% - defaultValues: array of default values
% - units: cell array of units, latex supported
%
% Returns:
% - parameters: mat array of parameters
%
% Example:
% labels = {'a', 'b', 'c'};
% defaultValues = [1, 2, 3];
% units = {'$m/s$', '$kg$', '$m^2$'};
%
% parameters = editParameters('test', labels, defaultValues, units);
% disp(parameters);
n = length(labels);

% No Units provided, set to dimensionless values
if nargin < 4
    units = num2cell(repmat("", 1, n));
end

assert(iscell(labels) , 'labels must be a cell array');
assert(iscell(units), 'units must be a cell array');
assert(isnumeric(defaultValues), 'defaultValues must be a numeric array');
assert(n == length(defaultValues), 'labels and defaultValues must have the same length');
assert(n == length(units), 'labels and units must have the same length');


parameters = defaultValues;
% Create a figure and axes
fig = uifigure('Position',[100 100 600 (n+1)*50]);
fig.Name = sprintf("%s (n=%d)", name, n);

layout = uigridlayout(fig);

layout.ColumnWidth = {'4x', '8x', '4x'};
layout.RowHeight = [repmat("1x", 1, n), "1.2x"];

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

end

