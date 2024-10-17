function mSub = subMatrix(m, row, col)
% 返回矩阵的上左部分，由参数`row`和`col`指定行数和列数

arguments
    m (:, :) 
    row (1,1) {mustBeInteger, mustBePositive, mustBeInSize(row, m, 1)}
    col (1,1) {mustBeInteger, mustBePositive, mustBeInSize(col, m, 2)} 
end

mSub = m(1:row, 1:col);

function mustBeInSize(idx, matrix, dim)
if idx > size(matrix, dim)
    eid = 'Size:outsize';
    msg = sprintf("Index (%d) must be less or equal to last index (%d) in given dimesion (%d)", idx, size(matrix, dim), dim);
    throwAsCaller(MException(eid, msg));
end
