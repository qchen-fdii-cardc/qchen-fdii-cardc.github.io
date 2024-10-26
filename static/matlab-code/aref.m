function cret = aref(c, i, j)
% AREF Get the element of an array
%
%   functional programming style helper function colleccions
%
%   cret = aref(c, i) returns the element of an array c at the index i.
%   cret = aref(c, i, j) returns the element of an array c at the index (i, j).
%
% Input arguments:
%   c (:, :)
%   i (1, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, i, 1)}
%   j (1, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, j, 2)} = 1
%
% Output arguments:
%   cret = cell(i, j)
%
% 2024 ©️ qchen2015@hotmail.com
arguments
    c (:, :)
    i (:, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, i, 1)}
    j (:, 1) {mustBeNonnegative, mustBeInteger, validateIdxInRange(c, j, 2)} = 1
end
cret = c(i, j);
end

