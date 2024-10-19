function [p, dp] = piecewiseFuncVec(x)
[p, dp] = arrayfun(@piecewiseFunc, x);
end