function s = bench_loop_sum_row(A)
    % 直接循环行，采用向量化的方式访问每个行
    % 直接对行调用sum函数求行和，最终累加求和
    % 内存：O(1)
    % 时间：O(m*n) ~ O(n^2)，这里同样认为.^ 的时间复杂度是O(n)
    s = 0;
    N = size(A, 1);

    for i = 1:N
        s = s + sum(A(i, :) .^ 2);
    end

end
