function plot_five_point_stencil_1d()
% Plot a 1D grid schematic for the 5-point finite-difference stencil.

close all;

outDir = fullfile(fileparts(mfilename('fullpath')), 'figures');
if ~exist(outDir, 'dir')
    mkdir(outDir);
end

pngPath = fullfile(outDir, 'five_point_stencil_1d.png');

n = 11;
i0 = 6; % center index for stencil demonstration

x = 1:n;
y = zeros(size(x));

fig = figure('Color', 'w', 'Position', [120, 120, 980, 360]);
ax = axes('Parent', fig);
hold(ax, 'on');

% Baseline and all grid points.
plot(ax, [x(1), x(end)], [0, 0], 'k-', 'LineWidth', 1.2);
plot(ax, x, y, 'ko', 'MarkerFaceColor', [0.95, 0.95, 0.95], 'MarkerSize', 8);

% Highlight stencil points i-2, i-1, i, i+1, i+2.
stIdx = (i0-2):(i0+2);
stColors = [0.20, 0.45, 0.80; 0.20, 0.65, 0.35; 0.85, 0.30, 0.20; 0.20, 0.65, 0.35; 0.20, 0.45, 0.80];
for k = 1:numel(stIdx)
    idx = stIdx(k);
    plot(ax, x(idx), 0, 'o', 'MarkerSize', 11, ...
        'MarkerFaceColor', stColors(k, :), 'MarkerEdgeColor', 'k');
end

% Labels for points.
lbl = {'x_{i-2}', 'x_{i-1}', 'x_i', 'x_{i+1}', 'x_{i+2}'};
for k = 1:numel(stIdx)
    idx = stIdx(k);
    text(ax, x(idx), 0.10, lbl{k}, 'HorizontalAlignment', 'center', ...
        'FontSize', 12, 'FontWeight', 'bold');
end

% Uniform spacing h indication.
for idx = (i0-2):(i0+1)
    yArrow = -0.07;
    quiver(ax, x(idx), yArrow, 1, 0, 0, 'Color', [0.35, 0.35, 0.35], ...
        'LineWidth', 1.1, 'MaxHeadSize', 0.18);
    text(ax, x(idx)+0.5, yArrow-0.02, 'h', 'HorizontalAlignment', 'center', ...
        'FontSize', 11, 'Color', [0.25, 0.25, 0.25]);
end

% Coefficients of the 5-point central stencil.
coefText = ['Central 5-point stencil for f''(x_i):  ' ...
    'f''(x_i) \approx (1/(12h)) [ f_{i-2} - 8f_{i-1} + 8f_{i+1} - f_{i+2} ]'];
text(ax, mean(x), -0.18, coefText, 'HorizontalAlignment', 'center', ...
    'FontSize', 12, 'Interpreter', 'tex');

title(ax, '1D Grid Schematic for the 5-Point Finite-Difference Stencil', 'FontSize', 14);
xlabel(ax, 'grid index');
ylabel(ax, '');

ax.YTick = [];
ax.XTick = 1:n;
ax.XLim = [0.5, n + 0.5];
ax.YLim = [-0.24, 0.16];
grid(ax, 'on');
ax.GridAlpha = 0.12;

exportgraphics(fig, pngPath, 'Resolution', 200);
fprintf('Figure saved: %s\n', pngPath);
end
