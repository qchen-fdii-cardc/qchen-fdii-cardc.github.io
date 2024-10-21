function m = sumMatrix(varargin)
% 产生一个特殊2维矩阵
%   其中矩阵的元素为，矩阵下标的和减去一
%   1 2 3 4 5 6 ...
%   2 3 4 5 6 7 ...
%   3 4 5 6 7 8 ...
%   4 5 6 7 8 9 ...
%   5 6 7 8 9 ...
%   6 7 8 9 ...

%  函数调用的参数，参考`zeros`, `ones`
%  Copyright @ qchen2015@hotmail.com

m = zeros(varargin{:});
sz = size(m);

for i = 1:numel(m)
    [row, col] = ind2sub(sz, i);
    m(row, col) = row + col - 1;
end