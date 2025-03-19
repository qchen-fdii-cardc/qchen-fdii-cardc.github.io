%% MOEA/D Comprehensive Example
% This script demonstrates how to use the MOEA/D algorithm
% with different test problems and decomposition approaches

clear;
clc;
close all;

%% Select test problem and decomposition approach
% Available problems:
%   'ZDT1', 'ZDT2', 'ZDT3', 'ZDT4', 'ZDT6' - Two-objective problems
%   'DTLZ1', 'DTLZ2', 'DTLZ3', 'DTLZ4'     - Many-objective problems
% 
% Available decomposition approaches:
%   'ws'  - Weighted sum
%   'tch' - Tchebycheff
%   'pbi' - Penalty-based boundary intersection

test_problem = 'ZDT1';
decomp_approach = 'tch';

%% Define problem parameters based on the selected test problem
switch upper(test_problem)
    case {'ZDT1', 'ZDT2', 'ZDT3', 'ZDT6'}
        problem.name = test_problem;
        problem.n_var = 30;      % Number of decision variables
        problem.var_lb = 0;      % Lower bound of decision variables
        problem.var_ub = 1;      % Upper bound of decision variables
        problem.n_obj = 2;       % Number of objectives
        
    case 'ZDT4'
        problem.name = test_problem;
        problem.n_var = 10;      % Number of decision variables
        problem.var_lb = zeros(1, problem.n_var);  % Lower bound of decision variables
        problem.var_ub = ones(1, problem.n_var);   % Upper bound of decision variables
        problem.var_lb(2:end) = -5;                % Different bounds for x2 to xn
        problem.var_ub(2:end) = 5;                 % Different bounds for x2 to xn
        problem.n_obj = 2;       % Number of objectives
        
    case {'DTLZ1', 'DTLZ2', 'DTLZ3', 'DTLZ4'}
        problem.name = test_problem;
        problem.n_obj = 3;       % Number of objectives
        problem.n_var = problem.n_obj + 7;  % Number of decision variables (k = 7)
        problem.var_lb = 0;      % Lower bound of decision variables
        problem.var_ub = 1;      % Upper bound of decision variables
        
    otherwise
        error('Unknown test problem: %s', test_problem);
end

% Define the evaluation function
problem.evaluate = @(x) test_problems(x, test_problem);

%% Set MOEA/D parameters
params.N = 100;           % Population size
params.T = 20;            % Number of neighbors
params.max_gen = 200;     % Maximum number of generations
params.cr = 1.0;          % Crossover rate
params.f = 0.5;           % Differential evolution parameter
params.nr = 2;            % Maximum number of solutions replaced by a child
params.decomp = decomp_approach;  % Decomposition approach
params.theta = 5.0;       % Parameter for PBI approach

%% Run MOEA/D
fprintf('Running MOEA/D on %s problem with %s decomposition approach...\n', ...
    test_problem, decomp_approach);

tic;
[population, F, EP] = moead(problem, params);
runtime = toc;

fprintf('Optimization completed in %.2f seconds\n', runtime);
fprintf('Number of non-dominated solutions found: %d\n', size(EP, 1));

%% Plot results
if problem.n_obj == 2
    % For two-objective problems
    figure;
    plot(F(:, 1), F(:, 2), 'bo', 'MarkerSize', 6);
    hold on;
    
    % If we have non-dominated solutions, plot them
    if ~isempty(EP)
        EP_F = zeros(size(EP, 1), 2);
        for i = 1:size(EP, 1)
            EP_F(i, :) = problem.evaluate(EP(i, :));
        end
        plot(EP_F(:, 1), EP_F(:, 2), 'ro', 'MarkerSize', 8, 'MarkerFaceColor', 'r');
    end
    
    % Plot the true Pareto front for ZDT problems if available
    switch upper(test_problem)
        case 'ZDT1'
            x = 0:0.01:1;
            y = 1 - sqrt(x);
            plot(x, y, 'k-', 'LineWidth', 2);
            
        case 'ZDT2'
            x = 0:0.01:1;
            y = 1 - x.^2;
            plot(x, y, 'k-', 'LineWidth', 2);
            
        case 'ZDT3'
            x = 0:0.005:1;
            y = 1 - sqrt(x) - x .* sin(10 * pi * x);
            % Filter out the discontinuous parts
            valid = y >= 0;
            plot(x(valid), y(valid), 'k-', 'LineWidth', 2);
    end
    
    xlabel('f_1');
    ylabel('f_2');
    title(sprintf('MOEA/D on %s with %s decomposition', test_problem, decomp_approach));
    legend('Final Population', 'Non-dominated Solutions', 'True Pareto Front');
    grid on;
    
elseif problem.n_obj == 3
    % For three-objective problems
    figure;
    scatter3(F(:, 1), F(:, 2), F(:, 3), 30, 'b', 'filled');
    hold on;
    
    % If we have non-dominated solutions, plot them
    if ~isempty(EP)
        EP_F = zeros(size(EP, 1), 3);
        for i = 1:size(EP, 1)
            EP_F(i, :) = problem.evaluate(EP(i, :));
        end
        scatter3(EP_F(:, 1), EP_F(:, 2), EP_F(:, 3), 50, 'r', 'filled');
    end
    
    xlabel('f_1');
    ylabel('f_2');
    zlabel('f_3');
    title(sprintf('MOEA/D on %s with %s decomposition', test_problem, decomp_approach));
    legend('Final Population', 'Non-dominated Solutions');
    grid on;
    view(45, 30);
end

%% Compare different decomposition approaches
if strcmp(decomp_approach, 'tch')  % Only run this section if the main run used Tchebycheff
    figure;
    
    % Define decomposition approaches to compare
    decomp_approaches = {'ws', 'tch', 'pbi'};
    decomp_names = {'Weighted Sum', 'Tchebycheff', 'PBI'};
    
    % Create subplots for each approach
    for i = 1:length(decomp_approaches)
        % Set decomposition approach
        params.decomp = decomp_approaches{i};
        
        % Run MOEA/D with fewer generations for quick comparison
        params.max_gen = 100;
        
        fprintf('Running MOEA/D with %s decomposition...\n', decomp_names{i});
        [~, F_comp, EP_comp] = moead(problem, params);
        
        % Plot results
        if problem.n_obj == 2
            subplot(1, 3, i);
            plot(F_comp(:, 1), F_comp(:, 2), 'bo', 'MarkerSize', 4);
            
            % If we have non-dominated solutions, plot them
            if ~isempty(EP_comp)
                EP_F_comp = zeros(size(EP_comp, 1), 2);
                for j = 1:size(EP_comp, 1)
                    EP_F_comp(j, :) = problem.evaluate(EP_comp(j, :));
                end
                hold on;
                plot(EP_F_comp(:, 1), EP_F_comp(:, 2), 'ro', 'MarkerSize', 6, 'MarkerFaceColor', 'r');
            end
            
            xlabel('f_1');
            ylabel('f_2');
            title(decomp_names{i});
            grid on;
            
        elseif problem.n_obj == 3
            subplot(1, 3, i);
            scatter3(F_comp(:, 1), F_comp(:, 2), F_comp(:, 3), 20, 'b', 'filled');
            
            % If we have non-dominated solutions, plot them
            if ~isempty(EP_comp)
                EP_F_comp = zeros(size(EP_comp, 1), 3);
                for j = 1:size(EP_comp, 1)
                    EP_F_comp(j, :) = problem.evaluate(EP_comp(j, :));
                end
                hold on;
                scatter3(EP_F_comp(:, 1), EP_F_comp(:, 2), EP_F_comp(:, 3), 30, 'r', 'filled');
            end
            
            xlabel('f_1');
            ylabel('f_2');
            zlabel('f_3');
            title(decomp_names{i});
            grid on;
            view(45, 30);
        end
    end
    
    sgtitle(sprintf('Comparison of Decomposition Approaches on %s', test_problem));
end 