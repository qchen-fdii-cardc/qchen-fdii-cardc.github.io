function s = bench_matrix_mul(A)
    % 直接将矩阵展开成一个向量，进行矩阵乘法计算
    s = A(:).' * A(:);
end
