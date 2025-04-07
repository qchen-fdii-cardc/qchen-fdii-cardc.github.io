%% define symbols
syms r h V(r,h) A(r, h)
V(r, h) = pi * r * r * h;
A(r, h) = 2 * pi * r * h + 2 * pi * r * r;

% 这两个很节省计算，也算是灵巧的小手，一定要学会约束条件提前给进去
assume(r > 0);
assume(h > 0);

constraint = V == 1;

%% solve h and subs to A
hr = solve(constraint, h);
Ar = subs(A, h, hr);

r_min = solve(diff(Ar, r) == 0);

assert(subs(diff(Ar, r, r), r, r_min) > 0);

%% report result
h_min = subs(hr, r_min);

pretty(simplify(h_min))
pretty(simplify(r_min))

%% solve with constraints
syms lambda
L(r, h, lambda) = A + lambda * (V - 1);

result = solve(diff(L, r)==0, diff(L, h)==0, diff(L, lambda)==0)