%% setup problem
n = round(logspace(3, 4, 10));
functions = {@bench_loop_column_sum, @bench_loop_sum_column, @bench_loop_row_sum, @bench_loop_sum_row, @bench_loop_vec, @bench_sum_sum, @bench_sum_all, @bench_sum_vec, @bench_vec_dot, @bench_matrix_mul};
markers = {'o', 'x', '+', '*', 's', 'd', '^', 'v', '>', '<', 'p', 'h'};

%% calculation
results = cell(numel(functions), 2);

for i = 1:numel(functions)
    [n, result] = bench_f_n(n, functions{i});
    results{i, 1} = n;
    results{i, 2} = result;
    fprintf('%s: %s: %s\n', func2str(functions{i}), mat2str(n), mat2str(result));
end

%% Visualize
figure;
clf;

for i = 1:numel(functions)
    [n, result] = results{i, :};
    plot(n, result, 'LineWidth', 2, 'Marker', markers{i});
    yscale('log');
    hold on;
    fprintf('%s: %s: %s\n', func2str(functions{i}), mat2str(n), mat2str(result));
end

legend(cellfun(@func2str, functions, 'UniformOutput', false), 'Location', 'BestOutSide', "interpreter", "none");
xlabel('Matrix size');
ylabel('Time (s)');

exportgraphics(gcf, '../matlab-img/compareSquareSum.png', 'Resolution', 600);

%% Benchmark functions helper
function [n, result] = bench_f_n(n, f)

    arguments
        n (:, :) {mustBePositive}
        f function_handle
    end

    n = round(n(:));
    result = zeros(1, numel(n));

    for i = 1:numel(n)
        A = rand(n(i), n(i));
        result(i) = timeit(@()f(A));
    end

end

%% Benchmark functions
function s = bench_loop_column_sum(A)
    v = 0;
    N = size(A, 2);

    for i = 1:N
        v = v + A(:, i) .^ 2;
    end

    s = sum(v);
end

function s = bench_loop_sum_column(A)
    s = 0;
    N = size(A, 2);

    for i = 1:N
        s = s + sum(A(:, i) .^ 2);
    end

end

function s = bench_loop_row_sum(A)
    v = 0;
    N = size(A, 1);

    for i = 1:N
        v = v + A(i, :) .^ 2;
    end

    s = sum(v);
end

function s = bench_loop_sum_row(A)
    s = 0;
    N = size(A, 1);

    for i = 1:N
        s = s + sum(A(i, :) .^ 2);
    end

end

function s = bench_loop_vec(A)
    s = 0;
    N = numel(A);

    for i = 1:N
        s = s + A(i) .^ 2;
    end

end

function s = bench_sum_sum(A)
    s = sum(sum(A .^ 2));
end

function s = bench_sum_all(A)
    s = sum(A .^ 2, 'all');
end

function s = bench_sum_vec(A)
    s = sum(A(:) .^ 2);
end

function s = bench_vec_dot(A)
    s = dot(A(:), A(:));
end

function s = bench_matrix_mul(A)
    s = A(:).' * A(:);
end
