function demo_fd_and_brachistochrone()
% Generate figures for high-order finite difference and brachistochrone BVP.

close all;
outDir = fullfile(fileparts(mfilename('fullpath')), 'figures');
if ~exist(outDir, 'dir')
    mkdir(outDir);
end

%% Part 1: derivative with 5-point, 4th-order matrix
f = @(x) sin(2*x) + x.^3 / 5;
df = @(x) 2*cos(2*x) + 3*x.^2 / 5;

xmin = 0;
xmax = 2;
n = 121;
[dfx, h, A, x, fx] = cd5p(f, xmin, xmax, n); %#ok<ASGLU>

dfExact = df(x);
err = dfx - dfExact;

fig1 = figure('Color', 'w', 'Position', [100, 100, 900, 420]);
subplot(1, 2, 1);
plot(x, dfExact, 'k-', 'LineWidth', 1.8);
hold on;
plot(x, dfx, 'r--', 'LineWidth', 1.4);
grid on;
xlabel('x');
ylabel('dy/dx');
title('dy/dx = A y / h 的数值导数结果');
legend('解析导数', '差分导数', 'Location', 'best');

subplot(1, 2, 2);
semilogy(x, abs(err) + eps, 'b-', 'LineWidth', 1.6);
grid on;
xlabel('x');
ylabel('|error|');
title(sprintf('n = %d, h = %.4g, max|e| = %.3e', n, h, max(abs(err))));

exportgraphics(fig1, fullfile(outDir, 'fd_derivative.png'), 'Resolution', 180);

fig2 = figure('Color', 'w', 'Position', [160, 120, 540, 460]);
imagesc(A);
axis image;
colormap(turbo);
colorbar;
title('五点四阶差分矩阵 A 的稀疏结构');
xlabel('j');
ylabel('i');
exportgraphics(fig2, fullfile(outDir, 'fd_matrix.png'), 'Resolution', 180);

%% Part 2: brachistochrone by FD + Newton linear solves
xb = 0;
xe = 2;
y0 = 1.2;
y1 = 0.2;
nb = 121;

[xbGrid, yb, info] = solve_brachistochrone_fd(xb, xe, y0, y1, nb);

fig3 = figure('Color', 'w', 'Position', [140, 90, 920, 420]);
subplot(1, 2, 1);
plot(xbGrid, yb, 'b-', 'LineWidth', 2.0);
set(gca, 'YDir', 'reverse');
grid on;
xlabel('x');
ylabel('y');
title('最速降线边值问题的有限差分解');

subplot(1, 2, 2);
A5 = cd5pA(nb);
hb = xbGrid(2) - xbGrid(1);
yp = (A5 * yb) / hb;
ypp = (A5 * (A5 * yb)) / (hb^2);
res = ypp + (1 + yp.^2) ./ (2 * yb);
semilogy(xbGrid, abs(res) + eps, 'm-', 'LineWidth', 1.6);
grid on;
xlabel('x');
ylabel('|R|');
title(sprintf('残差检验(%s): iter=%d, ||R||_{\\infty}=%.2e', info.method, info.iter, info.residualInf));

exportgraphics(fig3, fullfile(outDir, 'brachistochrone_solution.png'), 'Resolution', 180);

%% Part 3: compare FD solution with analytic cycloid
xc0 = 0;
xc1 = 2;
% For analytic cycloid comparison, start close to y=0 (classical normalization).
yc0 = 1.0e-3;
yc1 = 1.101;
nc = 161;

[xcGrid, ycNum, infoC] = solve_brachistochrone_fd(xc0, xc1, yc0, yc1, nc);
[theta, xAna, yAna, aCycloid] = brachistochrone_cycloid(xc0, xc1, yc0, yc1, nc); %#ok<ASGLU>
yAnaOnGrid = interp1(xAna, yAna, xcGrid, 'pchip');
errAna = ycNum - yAnaOnGrid;

fig4 = figure('Color', 'w', 'Position', [170, 100, 940, 430]);
subplot(1, 2, 1);
plot(xcGrid, yAnaOnGrid, 'k-', 'LineWidth', 2.0);
hold on;
plot(xcGrid, ycNum, 'r--', 'LineWidth', 1.6);
set(gca, 'YDir', 'reverse');
grid on;
xlabel('x');
ylabel('y');
title('最速降线: 五点差分解与摆线解析解对比');
legend('摆线解析解', '五点差分数值解', 'Location', 'best');

subplot(1, 2, 2);
semilogy(xcGrid, abs(errAna) + eps, 'b-', 'LineWidth', 1.6);
grid on;
xlabel('x');
ylabel('|y_{num}-y_{ana}|');
title(sprintf('解析对比误差: max=%.3e, iter=%d', max(abs(errAna)), infoC.iter));

exportgraphics(fig4, fullfile(outDir, 'brachistochrone_analytic_compare.png'), 'Resolution', 180);

fprintf('Figures saved in: %s\n', outDir);
fprintf('Brachistochrone Newton iter=%d, residualInf=%.3e\n', info.iter, info.residualInf);
fprintf('Cycloid compare: iter=%d, maxAbsErr=%.3e\n', infoC.iter, max(abs(errAna)));
end

function [theta, x, y, a] = brachistochrone_cycloid(x0, x1, y0, y1, n)
% Analytic brachistochrone curve (cycloid) between two points with y1>y0.

dx = x1 - x0;
dy = y1 - y0;
assert(dx > 0 && dy > 0, 'Cycloid comparison requires x1>x0 and y1>y0.');

r = dx / dy;
fun = @(th) (th - sin(th)) ./ (1 - cos(th)) - r;
theta1 = fzero(fun, [1e-6, 2*pi - 1e-6]);

a = dy / (1 - cos(theta1));
theta = linspace(0, theta1, n)';
x = x0 + a * (theta - sin(theta));
y = y0 + a * (1 - cos(theta));
end
