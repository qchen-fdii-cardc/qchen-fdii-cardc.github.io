function s = bench_sum_vec(A)
    % 直接将矩阵展开成一个向量
    % 进行元素.^计算，调用sum函数求和
    s = sum(A(:) .^ 2);
end
