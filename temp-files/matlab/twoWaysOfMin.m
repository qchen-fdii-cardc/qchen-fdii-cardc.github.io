min1 = @(m)min(m, [], 'all');
min2 = @(m)min(m(:));

% Sizes of the matrices
N = round(logspace(2, 4, 15));

% Time the two functions
times1 = arrayfun(timeFunc(min1), N);
times2 = arrayfun(timeFunc(min2), N);

% Plot the results
h = figure;
plot(N, times1, 'r-o', N, times2, 'k-+', LineWidth = 2);
legend(["min(m, [], 'all')", "min(m(:))"], Location = "best")
ylabel("Time(s)")
xlabel("Matrix Size")

exportgraphics(h, "comparison.png", Resolution = 300);

% Function to time a function
function func = timeFunc(f)
    func = @(n)timeitWithMatrixNxN(f, n);
end

% Function to time a function with a matrix of size NxN
function runtime = timeitWithMatrixNxN(minFunc, N)
    m = rand(N, N);
    runtime = timeit(@()feval(minFunc, m), 1);
end
