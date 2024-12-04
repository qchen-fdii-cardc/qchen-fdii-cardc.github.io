function s = bench_vec_dot(A)
    % 直接将矩阵展开成一个向量，进行点积计算
    s = dot(A(:), A(:));
end
