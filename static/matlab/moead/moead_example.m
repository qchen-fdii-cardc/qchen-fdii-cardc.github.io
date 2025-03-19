%% MOEA/D Example Script
% This script demonstrates how to use the MOEA/D algorithm
% on the ZDT1 test problem

clear;
clc;

%% Define the ZDT1 test problem
problem.name = 'ZDT1';
problem.n_var = 30;      % Number of decision variables
problem.var_lb = 0;       % Lower bound of decision variables
problem.var_ub = 1;       % Upper bound of decision variables
problem.n_obj = 2;        % Number of objectives

% Define the evaluation function for ZDT1
problem.evaluate = @(x) zdt1(x);

%% Set MOEA/D parameters
params.N = 100;           % Population size
params.T = 20;            % Number of neighbors
params.max_gen = 200;     % Maximum number of generations
params.cr = 1.0;          % Crossover rate
params.f = 0.5;           % Differential evolution parameter
params.nr = 2;            % Maximum number of solutions replaced by a child
params.decomp = 'tch';    % Decomposition approach ('ws', 'tch', 'pbi')
params.theta = 5.0;       % Parameter for PBI approach

%% Run MOEA/D
tic;
[population, F, EP] = moead(problem, params);
runtime = toc;

fprintf('Optimization completed in %.2f seconds\n', runtime);
fprintf('Number of non-dominated solutions found: %d\n', size(EP, 1));

%% Plot results
figure;
plot(F(:, 1), F(:, 2), 'bo', 'MarkerSize', 6);
hold on;

% If we have non-dominated solutions, plot them
if ~isempty(EP)
    EP_F = zeros(size(EP, 1), 2);
    for i = 1:size(EP, 1)
        EP_F(i, :) = zdt1(EP(i, :));
    end
    plot(EP_F(:, 1), EP_F(:, 2), 'ro', 'MarkerSize', 8, 'MarkerFaceColor', 'r');
end

% Plot the true Pareto front for ZDT1
x = 0:0.01:1;
y = 1 - sqrt(x);
plot(x, y, 'k-', 'LineWidth', 2);

xlabel('f_1');
ylabel('f_2');
title('MOEA/D on ZDT1');
legend('Final Population', 'Non-dominated Solutions', 'True Pareto Front');
grid on;

%% ZDT1 function
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
