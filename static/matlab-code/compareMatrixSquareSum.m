%% import functions in benchobjs
import benchobjs.*;

%% setup problem
n = round(logspace(3, 4, 10));
functions = {@bench_loop_row_column, @bench_loop_column_row, ...
                 @bench_loop_column_sum, @bench_loop_sum_column, ...
                 @bench_loop_row_sum, @bench_loop_sum_row, ...
                 @bench_loop_vec, @bench_sum_sum, ...
                 @bench_sum_all, @bench_sum_vec, ...
                 @bench_vec_dot, @bench_matrix_mul};
markers = {'o', 'x', '+', '*', 's', 'd', '^', 'v', '>', '<', 'p', 'h'};
% 这里是常见的小技巧，将函数的句柄存储在一个cell数组中
% 这样可以方便的对这些函数进行遍历
% 同时，把线型标签也存储在一个cell数组中，这样可以方便的对这些线型进行遍历

%% calculation
results = cell(numel(functions), 2);

for i = 1:numel(functions)
    [n, result] = bench_f_n(n, functions{i});
    results{i, 1} = n;
    results{i, 2} = result;
    fprintf('%s: %s: %s\n', func2str(functions{i}), ...
        mat2str(n), mat2str(result));
end

cmd = sprintf('save compareMatrixSquareSum%d  results', maxNumCompThreads);
eval(cmd);

% 这里试图考虑到多线程的影响，目前在12核心的及其上进行了不同线程数的测试
% 发现对于这个问题，多线程并没有展现出过大的差别
%% Visualize
% 一般也会把原始数据画出来稍微看一下
% 为了确保数据的正确性，并且可以对数据进行初步的分析
figure;
clf;

for i = 1:numel(functions)
    [n, result] = results{i, :};
    plot(n, result, 'LineWidth', 2, 'Marker', markers{i});
    yscale('log');
    hold on;
    fprintf('%s: %s: %s\n', func2str(functions{i}), ...
        mat2str(n), mat2str(result));
end

legend(cellfun(@(f)cellref(split(func2str(f), '.'), 2), ...
    functions, 'UniformOutput', false), ...
    'Location', 'BestOutSide', "interpreter", "none");
xlabel('Matrix size');
ylabel('Time (s)');
grid on

% exportgraphics(gcf, '../matlab-img/compareMatrixSquareSum-time.png', ...
%   'Resolution', 600);

%% Visualize 2
% 针对基准的加速比，这是最常见的基准测试结果的展示方式
figure;
clf;

for i = 2:numel(functions)
    [n, result] = results{i, :};
    plot(n, results{1, 2} ./ result, ...
        'LineWidth', 2, 'Marker', markers{i});
    hold on;
    fprintf('%s: %s: %s\n', func2str(functions{i}), ...
        mat2str(n), mat2str(result));
end

legend(cellfun(@(f)cellref(split(func2str(f), '.'), 2), ...
    functions(2:numel(functions)), 'UniformOutput', false), ...
    'Location', 'BestOutSide', "interpreter", "none");
xlabel('Matrix size');
ylabel('Time (s)');
grid on

% exportgraphics(gcf, '../matlab-img/compareMatrixSquareSum-acc.png', ...
%   'Resolution', 600);

%% Visualize 2
% 去掉基准和两个最好的函数，这样可以进行更加细节的比较和分析
% 这里必要性不太大，主要是为了展示这种方式
figure;
clf;

for i = 2:numel(functions) - 2
    [n, result] = results{i, :};
    plot(n, results{1, 2} ./ result, ...
        'LineWidth', 2, 'Marker', markers{i});
    hold on;
    fprintf('%s: %s: %s\n', func2str(functions{i}), ...
        mat2str(n), mat2str(result));
end

legend(cellfun(@(f)cellref(split(func2str(f), '.'), 2), ...
    functions(2:numel(functions) - 2), 'UniformOutput', false), ...
    'Location', 'BestOutSide', "interpreter", "none");
xlabel('Matrix size');
ylabel('Time (s)');
grid on

% exportgraphics(gcf, '../matlab-img/compareMatrixSquareSum-acc-2.png',...
%    'Resolution', 600);
