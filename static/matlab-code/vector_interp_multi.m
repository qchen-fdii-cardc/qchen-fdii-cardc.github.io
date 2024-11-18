function y_interp = vector_interp_multi(x_known, y_known, x_query)
    % VECTOR_INTERP_MULTI Interpolates a vector-valued function at multiple points.
    %
    % Inputs:
    %   x_known - A vector of known x values (1 x n)
    %   y_known - A matrix of known y values (N x n), where each column corresponds to a y value at the corresponding x
    %   x_query - A vector of x values at which to interpolate (1 x m)
    %
    % Output:
    %   y_interp - A matrix of interpolated y values (N x m), where each column corresponds to the interpolated y values for each x_query
    
    % Ensure the inputs are in the correct orientation
    x_known = x_known(:)'; % Row vector
    x_query = x_query(:)'; % Row vector
    [N, n] = size(y_known); % N is the dimension of y, n is the number of known points
    m = length(x_query); % Number of query points

    % Initialize the interpolated y values matrix
    y_interp = zeros(N, m);
    
    % Interpolate each dimension of y separately for each query point
    for i = 1:N
        y_interp(i, :) = interp1(x_known, y_known(i, :), x_query, 'linear');
    end
end