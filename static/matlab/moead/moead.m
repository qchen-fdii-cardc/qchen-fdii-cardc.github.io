function [population, F, EP] = moead(problem, params)
% MOEAD Multi-Objective Evolutionary Algorithm based on Decomposition
%
% Inputs:
%   problem - Structure containing problem information
%       .name       - Name of the problem
%       .n_var      - Number of decision variables
%       .var_lb     - Lower bound of decision variables
%       .var_ub     - Upper bound of decision variables
%       .n_obj      - Number of objectives
%       .evaluate   - Function handle to evaluate solutions
%
%   params - Structure containing algorithm parameters
%       .N          - Population size
%       .T          - Number of neighbors
%       .max_gen    - Maximum number of generations
%       .cr         - Crossover rate
%       .f          - Differential evolution parameter
%       .nr         - Maximum number of solutions replaced by a child
%       .decomp     - Decomposition approach ('ws', 'tch', 'pbi')
%       .theta      - Parameter for PBI approach
%
% Outputs:
%   population - Final population
%   F          - Objective values of final population
%   EP         - External population (non-dominated solutions)
%
% Reference:
%   Q. Zhang and H. Li, "MOEA/D: A Multiobjective Evolutionary Algorithm 
%   Based on Decomposition," in IEEE Transactions on Evolutionary Computation, 
%   vol. 11, no. 6, pp. 712-731, Dec. 2007.

% Default parameters if not provided
if ~isfield(params, 'N'), params.N = 100; end
if ~isfield(params, 'T'), params.T = 20; end
if ~isfield(params, 'max_gen'), params.max_gen = 300; end
if ~isfield(params, 'cr'), params.cr = 1.0; end
if ~isfield(params, 'f'), params.f = 0.5; end
if ~isfield(params, 'nr'), params.nr = 2; end
if ~isfield(params, 'decomp'), params.decomp = 'tch'; end
if ~isfield(params, 'theta'), params.theta = 5.0; end

% Extract problem parameters
n_var = problem.n_var;
var_lb = problem.var_lb;
var_ub = problem.var_ub;
n_obj = problem.n_obj;

% Generate weight vectors
W = generate_weight_vectors(params.N, n_obj);

% Initialize neighborhood
B = initialize_neighborhood(W, params.T);

% Initialize population
population = initialize_population(params.N, n_var, var_lb, var_ub);

% Evaluate initial population
F = zeros(params.N, n_obj);
for i = 1:params.N
    F(i, :) = problem.evaluate(population(i, :));
end

% Initialize ideal point
z = min(F, [], 1);

% Initialize external population
EP = [];

% Main loop
for gen = 1:params.max_gen
    for i = 1:params.N
        % Select mating pool
        mating_indices = B(i, randperm(params.T, 2));
        
        % Generate offspring
        y = differential_evolution_operator(population, i, mating_indices, params.cr, params.f, var_lb, var_ub);
        
        % Evaluate offspring
        fy = problem.evaluate(y);
        
        % Update ideal point
        z = min(z, fy);
        
        % Update neighbors
        c = 0;
        for j = B(i, randperm(params.T))
            if c >= params.nr
                break;
            end
            
            % Calculate decomposition values
            g_old = calculate_decomposition_value(F(j,:), z, W(j,:), params.decomp, params.theta);
            g_new = calculate_decomposition_value(fy, z, W(j,:), params.decomp, params.theta);
            
            % Update if better
            if g_new <= g_old
                population(j,:) = y;
                F(j,:) = fy;
                c = c + 1;
            end
        end
        
        % Update external population
        EP = update_external_population([EP; y], [problem.evaluate(EP); fy]);
    end
    
    % Display progress
    if mod(gen, 10) == 0
        fprintf('Generation %d/%d completed\n', gen, params.max_gen);
    end
end

end

function W = generate_weight_vectors(N, n_obj)
% Generate uniformly distributed weight vectors
if n_obj == 2
    % For bi-objective problems, use uniform distribution
    W = zeros(N, n_obj);
    for i = 1:N
        W(i, 1) = (i - 1) / (N - 1);
        W(i, 2) = 1 - W(i, 1);
    end
else
    % For more objectives, use Das and Dennis's method
    % This is a simplified version for 3 objectives
    % For more objectives, more sophisticated methods are needed
    W = zeros(N, n_obj);
    count = 1;
    
    % Determine step size based on population size
    H = floor((N)^(1/(n_obj-1)));
    
    % Generate combinations
    for i = 0:H
        if n_obj == 3
            for j = 0:H-i
                k = H - i - j;
                if count <= N
                    W(count, :) = [i, j, k] / H;
                    count = count + 1;
                end
            end
        else
            % For more than 3 objectives, this is a placeholder
            % In practice, you would need a more general approach
            if count <= N
                temp = zeros(1, n_obj);
                temp(1) = i / H;
                temp(n_obj) = (H - i) / H;
                W(count, :) = temp;
                count = count + 1;
            end
        end
    end
    
    % If we didn't generate enough weight vectors, fill the rest randomly
    if count <= N
        for i = count:N
            w = rand(1, n_obj);
            W(i, :) = w / sum(w);
        end
    end
end
end

function B = initialize_neighborhood(W, T)
% Initialize neighborhood based on weight vectors
[N, ~] = size(W);
B = zeros(N, T);

% Calculate distances between weight vectors
distances = zeros(N, N);
for i = 1:N
    for j = 1:N
        distances(i, j) = norm(W(i, :) - W(j, :));
    end
end

% Find T closest weight vectors for each weight vector
for i = 1:N
    [~, sorted_indices] = sort(distances(i, :));
    B(i, :) = sorted_indices(1:T);
end
end

function population = initialize_population(N, n_var, lb, ub)
% Initialize population randomly within bounds
if length(lb) == 1
    lb = repmat(lb, 1, n_var);
end
if length(ub) == 1
    ub = repmat(ub, 1, n_var);
end

population = zeros(N, n_var);
for i = 1:N
    population(i, :) = lb + (ub - lb) .* rand(1, n_var);
end
end

function y = differential_evolution_operator(population, i, mating_indices, cr, f, lb, ub)
% Differential evolution operator (DE/rand/1/bin)
[~, n_var] = size(population);

% Ensure lb and ub are vectors of length n_var
if length(lb) == 1
    lb = repmat(lb, 1, n_var);
end
if length(ub) == 1
    ub = repmat(ub, 1, n_var);
end

% Select parents
x1 = population(mating_indices(1), :);
x2 = population(mating_indices(2), :);
x = population(i, :);

% Generate offspring
y = x;
j_rand = randi(n_var);

for j = 1:n_var
    if rand <= cr || j == j_rand
        y(j) = x1(j) + f * (x2(j) - x(j));
        
        % Boundary handling
        if y(j) < lb(j)
            y(j) = lb(j) + rand * (ub(j) - lb(j));
        elseif y(j) > ub(j)
            y(j) = lb(j) + rand * (ub(j) - lb(j));
        end
    end
end
end

function g = calculate_decomposition_value(fx, z, lambda, approach, theta)
% Calculate decomposition value based on the chosen approach
switch approach
    case 'ws' % Weighted sum approach
        g = sum(lambda .* fx);
        
    case 'tch' % Tchebycheff approach
        g = max(lambda .* abs(fx - z));
        
    case 'pbi' % Penalty-based boundary intersection approach
        d1 = norm(fx - z) * cos(acos(sum((fx - z) .* lambda) / (norm(fx - z) * norm(lambda))));
        d2 = norm(fx - z) * sin(acos(sum((fx - z) .* lambda) / (norm(fx - z) * norm(lambda))));
        g = d1 + theta * d2;
        
    otherwise
        error('Unknown decomposition approach');
end
end

function EP = update_external_population(population, F)
% Update external population with non-dominated solutions
if isempty(population)
    EP = [];
    return;
end

% Find non-dominated solutions
n = size(F, 1);
is_dominated = false(n, 1);

for i = 1:n
    for j = 1:n
        if i ~= j
            if all(F(j, :) <= F(i, :)) && any(F(j, :) < F(i, :))
                is_dominated(i) = true;
                break;
            end
        end
    end
end

% Return non-dominated solutions
EP = population(~is_dominated, :);
end
