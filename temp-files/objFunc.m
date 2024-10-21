function res = objFunc(x)
arguments
    x (:,:) {mustBeTwo, mustBePositive, mustBeNumeric}
end

if size(x, 2) == 2
    x = x';
end

res = abs([-90-x(1, :)+x(2, :); x(1, :) - x(2, :) * 17]);



function mustBeTwo(matrix)

[m, n] = size(matrix);
if m ~=2 && n ~= 2
    eid = 'Size:wrongDim';
    msg = "Must be a 2 x N or N x 2 matrix";
    throwAsCaller(MException(eid, msg));
end
