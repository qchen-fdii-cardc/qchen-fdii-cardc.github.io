function rayleigh(mu, show_fig, y_1_0)
arguments
    mu (1,1) double {mustBeNumeric, mustBeFinite} = 0.4; % Default value for mu
    show_fig (1,1) logical = false; % Default value for show_fig
    y_1_0 (1,1) double {mustBeNumeric, mustBeFinite} = 0.001;
end

y0 = [y_1_0; 0]; % Initial conditions: y(0) = 0, dy/dt(0) = 1
tspan = [0 100]; % Time span for the solution


fig = figure('Units','inches', 'Position', [1, 1, 12, 6], 'Visible', false);

% [t, y] = ode45(@(t, y) rayleigh_ode(t, y, mu), tspan, y0);
[t, y] = ode45(@rayleigh_ode, tspan, y0, [], mu);

tl = tiledlayout(1,2);
title(tl, sprintf('Rayleigh ODE Solution mu = %.2f', mu));

nexttile
plot(t, y(:, 1))
xlabel('Time(s)')
ylabel('y')

nexttile
plot(y(:,1), y(:,2), 'b-', 'LineWidth', 2);
xlabel('y')
ylabel('y''')

exportgraphics(tl, sprintf('rayleigh_solution_%.2f.png', mu), 'Resolution', 500);

if show_fig
    set(fig, 'Visible', 'on');
else
    close(fig); % Close the figure if not showing
end
end

function dydt = rayleigh_ode(t, y, mu)
% Rayleigh ODE function
% dydt = rayleigh(t, y, mu)
% t: time variable
% y: state variable (y(1) = y, y(2) = dy/dt)
dydt = zeros(2, 1);
dydt(1) = y(2);
dydt(2) = mu * (1 - (1/3) * y(2)^2) * y(2) - y(1);
end