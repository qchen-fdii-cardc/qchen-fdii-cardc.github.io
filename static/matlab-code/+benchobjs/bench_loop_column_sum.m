function s = bench_loop_column_sum(A)
    % 直接循环列，采用向量化的方式访问每个列
    % 用一个向量来存储每一列和
    % 最终调用sum函数求和
    % 内存：O(n)
    % 时间：考虑到，向量加法的时间复杂度是O(n)
    % 所以，这里的时间复杂度依然是O(m*n) ~ O(n^2)
    N = size(A, 2);
    v = zeros(size(A, 1), 1);

    for i = 1:N
        v = v + A(:, i) .^ 2;
    end

    s = sum(v);
end
