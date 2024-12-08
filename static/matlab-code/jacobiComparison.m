%% Jacobi iteration
clear;
epsilon = 1; a = 1/2; n = 150; h = 1 / n;
A = tridiag(n, [epsilon; - (2 * epsilon + h); epsilon + h]);
b = zeros(n, 1);
b(1:n) = a * h * h;
x_init = zeros(n, 1);

%% calculation
for n = 1:20
    maxNumCompThreads(n);
    fprintf("maxNumCompThreads = %d\n", maxNumCompThreads);
    iter_times = round(logspace(3, 5, 10));
    t1 = arrayfun(@(n)timeit(@()iter_jacobi(A, b, x_init, 1.0e-7, n, 0), 2), iter_times);
    t2 = arrayfun(@(n)timeit(@()iter_jacobi(A, b, x_init, 1.0e-7, n, 1), 2), iter_times);
    save(sprintf("comaprison-%d", maxNumCompThreads), "t1", "t2");
    fprintf("i=1:n\t");
    fprintf("%.8f\t", t1);
    fprintf("\n");
    fprintf("i=1:n-1\t");
    fprintf("%.8f\t", t2);
    fprintf("\n");
end

%% visualize
figure;
plot(iter_times, t1, 'x-', iter_times, t2, 'o-')
legend({"i=1:n", "i=1:n-1"}, 'Location', 'best')
xlabel("iter_max")
ylabel("Time(s)")

%% maxNumCompThreads x iter_max

% calculate a matrix, row = numel(1:20), column = numel(iter_times)
% if t1(i, j) < t2(i, j), then 1, else 0

numCompThreads = 1:20;
iter_times = round(logspace(3, 5, 10));
comparison = zeros(numel(numCompThreads), numel(iter_times));

for i = 1:numel(numCompThreads)
    load(sprintf("comaprison-%d", i), "t1", "t2");
    comparison(i, :) = t1 < t2;
end

figure;

imagesc(comparison, [0, 1])
xlabel("iter_max", 'Interpreter', 'none')
xticklabels(iter_times)
ylabel("maxNumCompThreads")
yticks(numCompThreads)
yticklabels(numCompThreads)
title("t1 < t2")
grid on

exportgraphics(gca, "yellow_for_t1_le_t2.png")

%% calculate t1 and t2 for default maxNumCompThreads, 1000 times, iter_max = 100000

iter_times = 1000;
t1 = arrayfun(@(~)timeit(@()iter_jacobi(A, b, x_init, 1.0e-7, iter_times, 0), 2), 1:100);
t2 = arrayfun(@(~)timeit(@()iter_jacobi(A, b, x_init, 1.0e-7, iter_times, 1), 2), 1:100);

% there is no evidence that, t1 is significantly small than t2
% and the actual calculation is equivalent with i=1:n-1 and i=1:n
% since: j = i+1:n
