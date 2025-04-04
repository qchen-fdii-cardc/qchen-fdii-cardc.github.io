function [p,dp] = piecewiseFunc(x)
%piecewiseFunc
%    [P,DP] = piecewiseFunc(X)

%    This function was generated by the Symbolic Math Toolbox version 23.2.
%    2024-10-19 23:42:41

t2 = (x < 0.0);
if ~all(cellfun(@isscalar,{t2,x}))
    error(message('symbolic:sym:matlabFunction:ConditionsMustBeScalar'));
end
if (t2)
    p = x.^2-8.0;
elseif (0.0 <= x)
    p = -x;
else
    p = NaN;
end
if nargout > 1
    if ~all(cellfun(@isscalar,{t2,x}))    error(message('symbolic:sym:matlabFunction:ConditionsMustBeScalar'));end;if (t2)    dp = x.*2.0;elseif (0.0 < x)    dp = -1.0;else    dp = NaN;end;
end
end
