%% Benchmark functions helper
function [n, result] = bench_f_n(n, f)
    % 按照给定的参数，对函数进行测试
    % n: 矩阵大小
    % f: 函数句柄
    % 返回值：n, result
    % n: 矩阵大小的向量， result: 运行时间的向量
    arguments
        n (:, :) {mustBePositive}
        f function_handle
    end

    % 保证n是一个向量，并且是正整数
    n = round(n(:));
    result = zeros(1, numel(n));
    %  对每一个n进行测试，直接采用for循环，不使用arrayfun
    for i = 1:numel(n)
        A = rand(n(i), n(i));
        result(i) = timeit(@()f(A));
    end

    % result = arrayfun(@(x) timeit(@()f(rand(x, x))), n);
    % 这样也是可以的，但是，对于此处，上面的写法更加直观
    % 这就是采用for循环的地方
    % 为了代码的可读性，并且对性能的影响不大，因为这里的循环次数不多
end
