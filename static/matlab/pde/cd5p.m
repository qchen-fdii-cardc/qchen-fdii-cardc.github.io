
function [dfx, h, A, x, fx] = cd5p(func, xmin, xmax, n)

assert(n >= 5, "At least five point for 4-order method");
x = linspace(xmin, xmax, n)';
h = x(2) - x(1);

A = cd5pA(n);

fx = arrayfun(func, x);
dfx = (A * fx) / h;
end







