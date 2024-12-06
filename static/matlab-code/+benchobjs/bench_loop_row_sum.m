function s = bench_loop_row_sum(A)
    % 直接循环行，采用向量化的方式访问每行
    % 用一个向量来存储行和
    % 最终调用sum函数求和
    % 内存：O(n)
    % 时间：考虑到，向量加法的时间复杂度是O(n)
    % 所以，这里的时间复杂度依然是O(m*n) ~ O(n^2)
    N = size(A, 1);
    v = zeros(1, size(A, 2));

    for i = 1:N
        v = v + A(i, :) .^ 2;
    end

    s = sum(v);
end
