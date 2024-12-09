function fig = cadqeuryEditor(scriptFileName)
arguments
    scriptFileName (1,1) string = "mycadquery.cds"
end


%% prepare for cadquery, makesure the cadquery is installed
% consier to install the cadquery by using the following command
% !pip install cadquery
evalin('base', "cq = py.importlib.import_module('cadquery');");

%% create a uifigure
fig = uifigure('Name','CAD Query Editor', ...
    'NumberTitle','off'    );
% moving the figure to the center of the screen
fig.Position = [0,0, 1440, 800];
movegui(fig, 'center');
fig.Visible = 'off';

% create a grid layout
g = uigridlayout(fig, [1 2], "ColumnWidth", {'1x', '1x'});

%% left panel code editor
leftPanel = uipanel(g);
leftPanel.Title = 'Query Editor';
leftPanel.Layout.Row = 1;
leftPanel.Layout.Column = 1;

% create a grid layout for the left panel
% button row, fixed height
leftgrid = uigridlayout(leftPanel, [2 3], "RowHeight", {'1x', 40});

% create a code editor
code = uitextarea(leftgrid, ...
    'FontName', "Monospaced", ...
    'FontSize', 12, ...
    'Value', 'ret = cq.Workplane("front").box(1,1,1)');
code.Layout.Row = 1;
% span 3 columns
code.Layout.Column = [1,  3];

% call loading file to code
laodingFileToCode(code, scriptFileName);

% set the close request function to dump the code to a file
fig.CloseRequestFcn = @(~, ~)dumpCodeToFile(code, scriptFileName);

% add selections for workplane object
lst = uidropdown(leftgrid);
lst.Items = workPlanesInBase();
lst.Layout.Row=2;
lst.Layout.Column = 2;


% button do calculation and update lst
btnExec = uibutton(leftgrid, 'Text', 'Execute',...
    'ButtonPushedFcn', @(~, ~)executeCode(code, lst));
btnExec.Layout.Row = 2;
btnExec.Layout.Column = 1;

%% right panel axes to show the CAD model
rightPanel = uipanel(g);
rightPanel.Title = 'CAD Model';
rightPanel.FontSize = 12;
rightPanel.FontWeight = 'bold';

rightPanel.Layout.Row = 1;
rightPanel.Layout.Column = 2;

rightgrid = uigridlayout(rightPanel, [1,1]);

% add a axes to show the CAD model
ax = uiaxes(rightgrid);
ax.XLabel.String = 'X';
ax.YLabel.String = 'Y';
ax.ZLabel.String = 'Z';

% create a button to execute the code
btn = uibutton(leftgrid, 'Text', 'Show', ...
    'ButtonPushedFcn', @(src, evt)insert_pdegplot(lst, ax));
btn.Layout.Row = 2;
btn.Layout.Column = 3;

lst.ValueChangedFcn = @(src, evt)insert_pdegplot(lst, ax);
insert_pdegplot(lst, ax);

fig.Visible = 'on';
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% local functions
function laodingFileToCode(codeWidget, fn)
% if the file does not exist, return
if ~exist(fn, 'file')
    return;
end
% read the file content to the code widget
fid = fopen(fn, 'r');
code = fread(fid, '*char')';
fclose(fid);
codeWidget.Value = code;
end

function dumpCodeToFile(codeWidget, fn)
% if codeWidet has nothing than blank line return
if isempty(codeWidget.Value)
    return;
end

% write the code in the code widget to a file
fid = fopen(fn, 'w');
fprintf(fid, '%s', strjoin(codeWidget.Value, newline));
fclose(fid);

% close the figure
delete(gcbf);
end



function insert_pdegplot(wpLst, ax)
% get the workplane object from the list
% write the workplane object to a temp file
% import the geometry from the file
wpName = wpLst.Value;
fn = [tempname, '.stl'];
cmd = sprintf("%s.val().exportStl('%s');", wpName, fn);
evalin('base', cmd);

cla(ax);
gm = importGeometry(fn);
pdegplot(ax, gm, 'FaceLabels', 'on', 'FaceAlpha', 0.5);
end

function wps = workPlanesInBase()
vars = evalin('base', 'whos');
wp = vars(arrayfun(@(s)strcmp(s.class, 'py.cadquery.cq.Workplane'), vars));
wps = {wp.name};
end

function executeCode(codeWidget, lst)
% execute the code in current workspace

codeStr = codeWidget.Value;
wp0 = workPlanesInBase();
% combine cell array to a string
codeStr = strjoin(codeStr, newline);
evalin('base', sprintf("%s;", codeStr));

wp = workPlanesInBase();

if length(wp) == length(wp0)
    fprintf("No new workplane object added\n");
else
    % update lst
    lst.Items = wp;
end
end
