# MOEA/D: Multi-Objective Evolutionary Algorithm based on Decomposition

This is a MATLAB implementation of the MOEA/D algorithm for solving multi-objective optimization problems.

## Overview

MOEA/D is a state-of-the-art evolutionary algorithm for multi-objective optimization. It decomposes a multi-objective optimization problem into a number of scalar optimization subproblems and optimizes them simultaneously. Each subproblem is optimized by using information from its neighboring subproblems, which makes MOEA/D efficient and effective.

## Key Features

- Supports various decomposition approaches:
  - Weighted Sum (WS)
  - Tchebycheff (TCH)
  - Penalty-based Boundary Intersection (PBI)
- Uses Differential Evolution (DE) as the variation operator
- Maintains an external population of non-dominated solutions
- Handles any number of objectives (though most effective for 2-3 objectives)
- Easy to use with any multi-objective optimization problem

## Files

- `moead.m`: The main MOEA/D algorithm implementation
- `moead_example.m`: An example script demonstrating how to use MOEA/D on the ZDT1 test problem

## Usage

### Basic Usage

```matlab
% Define your problem
problem.name = 'YourProblem';
problem.n_var = 10;      % Number of decision variables
problem.var_lb = -5;     % Lower bound of decision variables
problem.var_ub = 5;      % Upper bound of decision variables
problem.n_obj = 2;       % Number of objectives
problem.evaluate = @your_evaluation_function;  % Function handle to evaluate solutions

% Set MOEA/D parameters
params.N = 100;          % Population size
params.T = 20;           % Number of neighbors
params.max_gen = 200;    % Maximum number of generations
params.decomp = 'tch';   % Decomposition approach ('ws', 'tch', 'pbi')

% Run MOEA/D
[population, F, EP] = moead(problem, params);

% Plot results
plot(F(:, 1), F(:, 2), 'bo');
```

### Evaluation Function

Your evaluation function should take a decision vector as input and return an objective vector:

```matlab
function F = your_evaluation_function(x)
    % x is a decision vector of size 1 x n_var
    % F is an objective vector of size 1 x n_obj
    
    % Example for a bi-objective problem
    f1 = ...  % Calculate first objective
    f2 = ...  % Calculate second objective
    
    F = [f1, f2];
end
```

## Parameters

### Problem Parameters

- `problem.name`: Name of the problem
- `problem.n_var`: Number of decision variables
- `problem.var_lb`: Lower bound of decision variables (scalar or vector)
- `problem.var_ub`: Upper bound of decision variables (scalar or vector)
- `problem.n_obj`: Number of objectives
- `problem.evaluate`: Function handle to evaluate solutions

### Algorithm Parameters

- `params.N`: Population size (default: 100)
- `params.T`: Number of neighbors (default: 20)
- `params.max_gen`: Maximum number of generations (default: 300)
- `params.cr`: Crossover rate for DE (default: 1.0)
- `params.f`: Scaling factor for DE (default: 0.5)
- `params.nr`: Maximum number of solutions replaced by a child (default: 2)
- `params.decomp`: Decomposition approach ('ws', 'tch', 'pbi') (default: 'tch')
- `params.theta`: Parameter for PBI approach (default: 5.0)

## Output

- `population`: Final population (matrix of size N x n_var)
- `F`: Objective values of final population (matrix of size N x n_obj)
- `EP`: External population of non-dominated solutions (matrix of size ? x n_var)

## Reference

Q. Zhang and H. Li, "MOEA/D: A Multiobjective Evolutionary Algorithm Based on Decomposition," in IEEE Transactions on Evolutionary Computation, vol. 11, no. 6, pp. 712-731, Dec. 2007.

## License

This code is provided under the MIT License. 