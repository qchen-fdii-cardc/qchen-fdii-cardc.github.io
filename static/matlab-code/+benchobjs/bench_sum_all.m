function s = bench_sum_all(A)
    % 直接调用一次sum函数，对矩阵进行求和
    s = sum(A .^ 2, 'all');
end
