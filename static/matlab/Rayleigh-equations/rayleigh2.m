%% Rayleigh Equation Phase Plot with ODE Output Function
function rayleigh2(mu, xy_lim, tspan, y0)

arguments
  mu (1,1) double = 0.5
  xy_lim (1,2) double = [2.5, 2.5]
  tspan (1,2) double = [0, 50]
  y0 (2,1) double = [0.1; 0]
end


% Define ODE function (Rayleigh oscillator)
odefun = @(t, y, mu) [0, 1; ...
  -1, mu * (1 - (1/3) * y(2)^2)] * y(:);

% Run ODE solver with custom OutputFcn for live phase plot and GIF export

refine_size = 10;
ode45(odefun, ...
  tspan, ...
  y0(:), ...
  odeset( ...
  'Refine', refine_size, ... 
  "OutputFcn", @(t, y, flag, varargin) ...
  ode_update_phase_plot(t, y, flag, 1, xy_lim(1), xy_lim(2), mu, refine_size)), ...
  mu);

end
%% Output function for live phase plot and GIF export
function status = ode_update_phase_plot(t, y, flag, figure_no, xlim_value, ylim_value, mu, refine_size)
status = 0;
gif_name = sprintf('rayleigh_mu=%.2f.gif', mu);

% Export final image when ODE is done
if isequal(flag, "done")
  figure(figure_no);
  exportgraphics(gcf, sprintf("rayleigh2-phase_plot mu=%.2f.png", mu), "Resolution", 300);
  return;
end

fig_title = sprintf("Phase Plot (mu=%.2f) @ %.2fs", mu, t(end));

% Initialize phase plot
if isequal(flag, "init")
  fprintf("Initializing phase plot...\n");
  figure(figure_no); clf;
  plot(y(1), y(2), '-b', 'Tag', "phase_line");
  hold on;
  plot(y(1), y(2), '-or', 'Tag', "phase_comet", 'MarkerSize', 5);
  xlabel("y_1");
  ylabel("y_2");
  title(fig_title);
  grid on; axis equal;
  xlim([-xlim_value, xlim_value]);
  ylim([-ylim_value, ylim_value]);
  exportgraphics(gcf, gif_name, 'Append', false);
  return;
end

% Update phase plot during integration
if isempty(flag)
  figure(figure_no);
  
  % Update main trajectory line
  phase_line = findobj(gcf, "Type", "line", 'Tag', 'phase_line');
  if isempty(phase_line), status = 1; return; end
  xx = get(phase_line, "XData");
  yy = get(phase_line, "YData");
  new_xx = [xx(:); y(1, :)'];
  new_yy = [yy(:); y(2, :)'];
  set(phase_line, "XData", new_xx, "YData", new_yy);
  
  % Update comet tail (last 10 points)
  comet_line = findobj(gcf, "Type", "line", 'Tag', 'phase_comet');
  if isempty(comet_line), status = 1; return; end
  tail_len = min(numel(new_xx), refine_size) - 1;
  set(comet_line, "XData", new_xx(end-tail_len:end), "YData", new_yy(end-tail_len:end));
  
  % Update plot appearance
  title(fig_title);
  grid on; axis equal;
  xlim([-xlim_value, xlim_value]);
  ylim([-ylim_value, ylim_value]);
  
  % Export current frame to GIF
  exportgraphics(gcf, gif_name, 'Append', true);
end
end