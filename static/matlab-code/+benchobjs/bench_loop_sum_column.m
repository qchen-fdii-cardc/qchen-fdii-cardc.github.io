function s = bench_loop_sum_column(A)
    % 直接循环列，采用向量化的方式访问每个列
    % 直接对列调用sum函数求列和，最终累加求和
    % 内存：O(1)
    % 时间：O(m*n) ~ O(n^2)，这里同样认为.^ 的时间复杂度是O(n)
    s = 0;
    N = size(A, 2);

    for i = 1:N
        s = s + sum(A(:, i) .^ 2);
    end

end
