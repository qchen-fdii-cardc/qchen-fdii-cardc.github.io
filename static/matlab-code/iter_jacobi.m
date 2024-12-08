function [x_iter, iter_times] = iter_jacobi(A, b, x_init, tol, iter_max, flag)

    if nargin < 6
        flag = 0;
    end

    if ~ismember(flag, [0, 1])
        flag = 0;
    end

    % iter_jacobi - Solve the system of linear equations of Ax = b using jacobi
    % iteration method.
    %
    % Syntax: [x_iter, iter_times] = iter_jacobi(A, b, x_init, tol, iter_max)
    %
    % Jacobi iteration method:
    % Let A be a square matrix, we decompose A as A = D - L - U, then we have
    % the iterative scheme below:
    % x_k = B x_k-1 + g,
    % where B = D^-1 (L + U), g = D^-1 b.
    %
    % Input:
    % A - Coefficient matrix.
    % b - Column vector.
    % x_init - Initial guess for the solution.
    % tol - Tolerance for the stopping criterion.
    % iter_max - Maximum number of iterations.
    %
    % Output:
    % x_iter - Numerical solution approxiate to the true solution.
    % iter_times - The times of iterations performed.

    % Get the size of the matrix A and b, then check whether A is a square,
    % and if the dimension of b matches A.
    [m, n] = size(A);
    assert(m == n, 'Matrix U must be square.');
    assert(size(b, 1) == n, 'Dimension of b must match U.');

    % Initialize Jacobi iterative matrix B, vector g and x_iter
    B = zeros(n);
    g = zeros(n, 1);
    x_iter = x_init;

    % Calculate the Jacobi iterative matrix B and the vector g
    for i = 1:n

        if A(i, i) == 0
            error('Zero pivot encoutered.');
        end

        for j = i + 1:n
            B(i, j) =- A(i, j) / A(i, i);
            B(j, i) =- A(j, i) / A(i, i);
        end

        g(i) = b(i) / A(i, i);
    end

    q = norm(B);

    if q >= 1
        error('Jacobi iteration method does not converge.');
    end

    iEnd = n - flag;
    % Perform iteration
    for iter_times = 1:iter_max

        x_old = x_iter;
        x_iter = g;

        for i = 1:iEnd

            for j = i + 1:n
                x_iter(i) = x_iter(i) + B(i, j) * x_old(j);
                x_iter(j) = x_iter(j) + B(j, i) * x_old(i);
            end

        end

        e = norm(x_iter - x_old);

        if q / (1 - q) * e < tol
            break;
        end

    end

end

function A = tridiag(N, v)
    % tridiag - Generate a tridiagonal matrix.
    %
    % Syntax: A = tridiag(N, v)
    %
    % Input:
    % N - dimension of the tridiagonal matrix.
    % v - Tri-dimensional vector.
    %
    % Output:
    % A - Tridiagonal matrix with the dimension of N.

    A = zeros(N);

    for i = 1:N - 1
        A(i + 1, i) = v(1);
        A(i, i) = v(2);
        A(i, i + 1) = v(3);
    end

    A(N, N) = v(2);

end
