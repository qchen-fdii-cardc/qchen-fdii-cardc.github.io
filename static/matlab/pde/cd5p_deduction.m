

%% left-1

A1 = ones(5, 1);
A2base = [0, 1, 2, 3, 4]';
A = [A1, A2base, A2base.^2 ./ factorial(2), A2base.^3 ./ factorial(3), A2base.^4 / factorial(4)];
b = [0, 1, 0, 0, 0];
[num, den] = rat(b / A);
k = b / A

%% left-2
A1 = ones(5, 1);
A2base = [-1, 0, 1, 2, 3]';
A = [A1, A2base, A2base.^2 ./ factorial(2), A2base.^3 ./ factorial(3), A2base.^4 / factorial(4)];
b = [0, 1, 0, 0, 0];
[num, den] = rat(b / A);
k = b / A

%% center total
A1 = ones(5, 1);
A2base = [-2, -1, 0, 1, 2]';
A = [A1, A2base, A2base.^2./factorial(2), A2base.^3./factorial(3), A2base.^4./factorial(4)];
b = [0, 1, 0, 0, 0];
[num, den] = rat(b / A);
k = b / A

% fdof = @(f, x, h)k * [f(x-2*h), f(x-h), f(x), f(x+h), f(x+2*h)]' / h;



%% righ-1
A1 = ones(5, 1);
A2base = [-3, -2, -1, 0, 1]';
A = [A1, A2base, A2base.^2 ./ factorial(2), A2base.^3 ./ factorial(3), A2base.^4 / factorial(4)];
b = [0, 1, 0, 0, 0];
[num, den] = rat(b / A);
k = b / A

%% righ-2
A1 = ones(5, 1);
A2base = [-4, -3, -2, -1, 0]';
A = [A1, A2base, A2base.^2 ./ factorial(2), A2base.^3 ./ factorial(3), A2base.^4 / factorial(4)];
b = [0, 1, 0, 0, 0];
[num, den] = rat(b / A);
k = b / A
