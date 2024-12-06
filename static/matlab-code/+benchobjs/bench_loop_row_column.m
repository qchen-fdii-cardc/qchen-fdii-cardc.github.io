function sumRet = bench_loop_row_column(A)
    % 按照行、列的方式进行循环求和，基线算法
    % 内存：O(1)
    % 时间：O(m*n) ~ O(n^2)
    [m, n] = size(A);
    sumRet = 0;
    % 对行进行循环
    for i = 1:m
        % 对列进行循环
        for j = 1:n
            sumRet = sumRet + A(i, j) ^ 2;
        end

        % 列循环结束
    end

    % 行循环结束，
end
