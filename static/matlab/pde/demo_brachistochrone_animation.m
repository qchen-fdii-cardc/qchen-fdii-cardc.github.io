function demo_brachistochrone_animation()
% Create a GIF animation comparing descent on typical curves.
% Curves: straight line, concave parabola, cycloid, and a more-concave cubic.

close all;

outDir = fullfile(fileparts(mfilename('fullpath')), 'figures');
if ~exist(outDir, 'dir')
    mkdir(outDir);
end

gifPath = fullfile(outDir, 'brachistochrone_compare.gif');

g = 9.81;
x0 = 0.0;
x1 = 2.0;
y0 = 0.0;
y1 = 1.1;
n = 500;

x = linspace(x0, x1, n)';
s = (x - x0) / (x1 - x0);

% 1) Straight line
curve(1).name = 'Line';
curve(1).x = x;
curve(1).y = y0 + (y1 - y0) * s;

% 2) Quadratic arc (concave-down, steeper at start then flatter)
curve(2).name = 'Parabola';
curve(2).x = x;
curve(2).y = y0 + (y1 - y0) * (2*s - s.^2);

% 3) Cycloid (analytic brachistochrone)
curve(3).name = 'Cycloid';
curve(3).x = x;
curve(3).y = cycloid_y_on_x(x, x0, x1, y0, y1);

% 4) More-concave curve than cycloid (same endpoints, lower than cycloid)
curve(4).name = 'SuperConcave';
curve(4).x = x;
dyTot = y1 - y0;
bulge = 0.30 * dyTot * s .* (1 - s); % stronger mid-span sag for clearer contrast
curve(4).y = curve(3).y + bulge;

colors = [0.15 0.35 0.80; 0.10 0.60 0.25; 0.85 0.20 0.20; 0.45 0.15 0.75];

% Precompute travel-time maps t(x) for each curve.
for k = 1:numel(curve)
    [curve(k).t, curve(k).tEnd] = travel_time_map(curve(k).x, curve(k).y, g, y0);
end

tMax = max([curve.tEnd]);

fig = figure('Color', 'w', 'Position', [120, 100, 980, 520]);
ax = axes('Parent', fig);
hold(ax, 'on');

for k = 1:numel(curve)
    plot(ax, curve(k).x, curve(k).y, '-', 'LineWidth', 2.2, 'Color', colors(k, :));
end

% Start and end points
plot(ax, x0, y0, 'ko', 'MarkerFaceColor', 'k', 'MarkerSize', 6);
plot(ax, x1, y1, 'ko', 'MarkerFaceColor', 'k', 'MarkerSize', 6);

% Moving markers
for k = 1:numel(curve)
    curve(k).hBall = plot(ax, x0, y0, 'o', ...
        'MarkerSize', 9, ...
        'MarkerFaceColor', colors(k, :), ...
        'MarkerEdgeColor', 'k');
end

set(ax, 'YDir', 'reverse');
grid(ax, 'on');
axis(ax, [x0 - 0.05, x1 + 0.05, y0 - 0.02, y1 + 0.12]);
xlabel(ax, 'x');
ylabel(ax, 'y (downward positive)');
title(ax, 'Brachistochrone Comparison: Descent on Different Curves');

legendLabels = strings(1, numel(curve));
for k = 1:numel(curve)
    legendLabels(k) = sprintf('%s: T=%.3f s', curve(k).name, curve(k).tEnd);
end
legend(ax, legendLabels, 'Location', 'best');

timeText = text(ax, x0 + 0.05, y0 + 0.08, 't = 0.000 s', 'FontSize', 12, 'FontWeight', 'bold');

nFrames = 220;
delay = 0.03;

for f = 1:nFrames
    tau = (f - 1) / (nFrames - 1);
    tNow = tau * tMax;
    
    for k = 1:numel(curve)
        tClamped = min(tNow, curve(k).tEnd);
        xNow = interp1(curve(k).t, curve(k).x, tClamped, 'pchip');
        yNow = interp1(curve(k).t, curve(k).y, tClamped, 'pchip');
        set(curve(k).hBall, 'XData', xNow, 'YData', yNow);
    end
    
    set(timeText, 'String', sprintf('t = %.3f s', tNow));
    drawnow;
    
    frame = getframe(fig);
    img = frame2im(frame);
    [A, map] = rgb2ind(img, 256);
    
    if f == 1
        imwrite(A, map, gifPath, 'gif', 'LoopCount', Inf, 'DelayTime', delay);
    else
        imwrite(A, map, gifPath, 'gif', 'WriteMode', 'append', 'DelayTime', delay);
    end
end

fprintf('GIF saved: %s\n', gifPath);
for k = 1:numel(curve)
    fprintf('%s total time: %.6f s\n', curve(k).name, curve(k).tEnd);
end
end

function y = cycloid_y_on_x(xQuery, x0, x1, y0, y1)
% Compute cycloid y(x) by solving endpoint parameter and interpolating.

dx = x1 - x0;
dy = y1 - y0;
assert(dx > 0 && dy > 0, 'Require x1>x0 and y1>y0 for this setup.');

r = dx / dy;
fun = @(th) (th - sin(th)) ./ (1 - cos(th)) - r;
theta1 = fzero(fun, [1e-6, 2*pi - 1e-6]);
a = dy / (1 - cos(theta1));

theta = linspace(0, theta1, 4000)';
x = x0 + a * (theta - sin(theta));
y = y0 + a * (1 - cos(theta));

y = interp1(x, y, xQuery, 'pchip');
end

function [t, tEnd] = travel_time_map(x, y, g, yStart)
% Build cumulative travel time t(x) using dt = ds / sqrt(2g(y-yStart)).

dx = diff(x);
dy = diff(y);
ds = sqrt(dx.^2 + dy.^2);

yMid = 0.5 * (y(1:end-1) + y(2:end));
head = max(yMid - yStart, 1e-12);
vMid = sqrt(2 * g * head);
dt = ds ./ vMid;

t = [0; cumsum(dt)];
tEnd = t(end);
end
