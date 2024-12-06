function s = bench_loop_vec(A)
    % 直接将矩阵展开成一个向量，循环求和
    % 内存：O(1)
    % 时间：O(m*n) ~ O(n^2)
    s = 0;
    N = numel(A);

    for i = 1:N
        s = s + A(i) .^ 2;
    end

end
