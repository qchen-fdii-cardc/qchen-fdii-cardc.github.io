function sumRet = bench_loop_column_row(A)
    % 按照列、行的方式进行循环求和，第一次改进的算法
    % 充分考虑到矩阵的存储方式：列优先
    % 内存：O(1)
    % 时间：O(m*n) ~ O(n^2)
    [m, n] = size(A);
    sumRet = 0;

    for i = 1:n

        for j = 1:m
            sumRet = sumRet + A(j, i) ^ 2;
        end

    end

end
