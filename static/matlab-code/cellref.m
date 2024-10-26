function cret = cellref(c, i, j)
% CELLREF Get the element of a cell array
%
%   functional programming style helper function colleccions
%
%   cret = cellref(c, i) returns the element of a cell array c at the index i.
%   cret = cellref(c, i, j) returns the element of a cell array c at the index (i, j).
%
% Input arguments:
%   c (:, :) cell
%   i (1, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, i, 1)}
%   j (1, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, j, 2)} = 1
%
% Output arguments:
%   cret = cell{i, j}
%
% 2024 ©️ qchen2015@hotmail.com
arguments
    c (:, :) cell
    i (1, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, i, 1)}
    j (1, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, j, 2)} = 1
end
cret = c{i, j};
end

