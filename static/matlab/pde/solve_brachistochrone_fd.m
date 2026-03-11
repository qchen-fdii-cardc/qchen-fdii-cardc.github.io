function [x, y, info] = solve_brachistochrone_fd(xmin, xmax, y0, y1, n)
% Solve brachistochrone BVP by 5-point finite differences and Newton.
% ODE: y'' = -(1 + (y')^2) / (2y), with fixed endpoints.

assert(n >= 3, "Need at least 3 grid points.");
assert(y0 > 0 && y1 > 0, "Endpoint y must be positive for this normalized model.");

x = linspace(xmin, xmax, n)';
h = x(2) - x(1);

% Initial guess: smooth monotone profile staying positive.
t = (x - xmin) / (xmax - xmin);
y = y0 + (y1 - y0) * t + 0.05 * sin(pi * t);
y = max(y, 1e-3);

maxIter = 40;
tol = 1e-10;
damp = 1.0;

% Build the 5-point first derivative matrix and its square for y''.
A = cd5pA(n);
D1 = A / h;
D2 = (A * A) / (h^2);

idxInt = 2:n-1;
uCols = 2:n-1;
idxBnd = [1, n];

D1i = D1(idxInt, :);
D2i = D2(idxInt, :);
D1u = D1i(:, uCols);
D2u = D2i(:, uCols);

for iter = 1:maxIter
    u = y(idxInt);
    yp = D1i * y;
    ypp = D2i * y;
    
    F = ypp + (1 + yp.^2) ./ (2 * u);
    
    % Jacobian of F(u): D2u + d[(1+yp^2)/(2u)]/du
    J = D2u + diag(yp ./ u) * D1u - diag((1 + yp.^2) ./ (2 * u.^2));
    
    delta = -J \ F;
    
    yTrial = y;
    yTrial(idxInt) = y(idxInt) + damp * delta;
    yTrial(idxBnd) = [y0; y1];
    
    while any(yTrial <= 0)
        damp = 0.5 * damp;
        if damp < 1e-6
            break;
        end
        yTrial(idxInt) = y(idxInt) + damp * delta;
        yTrial(idxBnd) = [y0; y1];
    end
    
    y = yTrial;
    
    if norm(F, inf) < tol && norm(delta, inf) < tol
        break;
    end
end

info.iter = iter;
info.residualInf = norm(F, inf);
info.stepInf = norm(delta, inf);
info.h = h;
info.method = "5-point D1/D2 matrix + Newton";
end
