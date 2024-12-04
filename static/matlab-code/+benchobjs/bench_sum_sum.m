function s = bench_sum_sum(A)
    % 直接两次调用sum函数，对矩阵进行求和
    s = sum(sum(A .^ 2));
end
