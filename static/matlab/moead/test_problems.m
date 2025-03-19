function F = test_problems(x, problem_name)
% TEST_PROBLEMS Collection of common multi-objective test problems
%
% Inputs:
%   x           - Decision vector
%   problem_name - Name of the test problem
%
% Outputs:
%   F           - Objective vector
%
% Available problems:
%   'ZDT1', 'ZDT2', 'ZDT3', 'ZDT4', 'ZDT6' - Two-objective problems
%   'DTLZ1', 'DTLZ2', 'DTLZ3', 'DTLZ4'     - Many-objective problems

switch upper(problem_name)
    case 'ZDT1'
        F = zdt1(x);
    case 'ZDT2'
        F = zdt2(x);
    case 'ZDT3'
        F = zdt3(x);
    case 'ZDT4'
        F = zdt4(x);
    case 'ZDT6'
        F = zdt6(x);
    case 'DTLZ1'
        F = dtlz1(x);
    case 'DTLZ2'
        F = dtlz2(x);
    case 'DTLZ3'
        F = dtlz3(x);
    case 'DTLZ4'
        F = dtlz4(x);
    otherwise
        error('Unknown test problem: %s', problem_name);
end
end

%% ZDT Test Suite (Two-objective problems)

function F = zdt1(x)
% ZDT1 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x 2

n = length(x);

% First objective
f1 = x(1);

% Second objective
g = 1 + 9 * sum(x(2:n)) / (n - 1);
h = 1 - sqrt(f1 / g);
f2 = g * h;

F = [f1, f2];
end

function F = zdt2(x)
% ZDT2 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x 2

n = length(x);

% First objective
f1 = x(1);

% Second objective
g = 1 + 9 * sum(x(2:n)) / (n - 1);
h = 1 - (f1 / g)^2;
f2 = g * h;

F = [f1, f2];
end

function F = zdt3(x)
% ZDT3 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x 2

n = length(x);

% First objective
f1 = x(1);

% Second objective
g = 1 + 9 * sum(x(2:n)) / (n - 1);
h = 1 - sqrt(f1 / g) - (f1 / g) * sin(10 * pi * f1);
f2 = g * h;

F = [f1, f2];
end

function F = zdt4(x)
% ZDT4 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x 2

n = length(x);

% First objective
f1 = x(1);

% Second objective
g = 1 + 10 * (n - 1);
for i = 2:n
    g = g + (x(i)^2 - 10 * cos(4 * pi * x(i)));
end

h = 1 - sqrt(f1 / g);
f2 = g * h;

F = [f1, f2];
end

function F = zdt6(x)
% ZDT6 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x 2

n = length(x);

% First objective
f1 = 1 - exp(-4 * x(1)) * (sin(6 * pi * x(1)))^6;

% Second objective
g = 1 + 9 * (sum(x(2:n)) / (n - 1))^0.25;
h = 1 - (f1 / g)^2;
f2 = g * h;

F = [f1, f2];
end

%% DTLZ Test Suite (Many-objective problems)

function F = dtlz1(x)
% DTLZ1 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x M (default M = 3)

n = length(x);
M = 3;  % Number of objectives (default: 3)
k = n - M + 1;

% Calculate g
g = 100 * (k + sum((x(M:n) - 0.5).^2 - cos(20 * pi * (x(M:n) - 0.5))));

% Calculate objectives
F = zeros(1, M);
for i = 1:M
    F(i) = 0.5 * (1 + g);
    for j = 1:M-i
        F(i) = F(i) * x(j);
    end
    if i > 1
        F(i) = F(i) * (1 - x(M-i+1));
    end
end
end

function F = dtlz2(x)
% DTLZ2 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x M (default M = 3)

n = length(x);
M = 3;  % Number of objectives (default: 3)
k = n - M + 1;

% Calculate g
g = sum((x(M:n) - 0.5).^2);

% Calculate objectives
F = zeros(1, M);
for i = 1:M
    F(i) = (1 + g);
    for j = 1:M-i
        F(i) = F(i) * cos(x(j) * pi/2);
    end
    if i > 1
        F(i) = F(i) * sin(x(M-i+1) * pi/2);
    end
end
end

function F = dtlz3(x)
% DTLZ3 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x M (default M = 3)

n = length(x);
M = 3;  % Number of objectives (default: 3)
k = n - M + 1;

% Calculate g (same as DTLZ1)
g = 100 * (k + sum((x(M:n) - 0.5).^2 - cos(20 * pi * (x(M:n) - 0.5))));

% Calculate objectives (same as DTLZ2)
F = zeros(1, M);
for i = 1:M
    F(i) = (1 + g);
    for j = 1:M-i
        F(i) = F(i) * cos(x(j) * pi/2);
    end
    if i > 1
        F(i) = F(i) * sin(x(M-i+1) * pi/2);
    end
end
end

function F = dtlz4(x)
% DTLZ4 test problem
% Input: x - Decision vector of size 1 x n_var
% Output: F - Objective vector of size 1 x M (default M = 3)

n = length(x);
M = 3;  % Number of objectives (default: 3)
k = n - M + 1;
alpha = 100;  % Parameter controlling the density of points on the Pareto front

% Calculate g
g = sum((x(M:n) - 0.5).^2);

% Calculate objectives
F = zeros(1, M);
for i = 1:M
    F(i) = (1 + g);
    for j = 1:M-i
        F(i) = F(i) * cos(x(j)^alpha * pi/2);
    end
    if i > 1
        F(i) = F(i) * sin(x(M-i+1)^alpha * pi/2);
    end
end
end 