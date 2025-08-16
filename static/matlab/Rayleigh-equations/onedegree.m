odefun = @(t, y, m, mu)[
    0, 1;
    -mu/m, 0
    ] * y(:);


figure
m = 1;
legends = {};
n = 3;
colors = lines(n);
mus = logspace(-1, 1, n);
u0 = 1; v0 = 0.5;
for muidx = 1:n
    mu = mus(muidx);
    hold on
    tau = 2 * pi * sqrt(m / mu);
    ode45(@(t, y, varargin) ...
        odefun(t, y, varargin{:}), ...
        [0,  tau], [u0; v0], ...
        odeset('OutputFcn', @odephas2, ...
        'Refine', 10), ...
        m, mu);
    legends = [legends; {sprintf('$\\mu=%.2f$', mu)}];
    axis equal
    xlabel('$u$', 'Interpreter', 'latex')
    ylabel('$\dot{u}$', 'Interpreter', 'latex')
    % set color of latest added line
    ud = get(gcf,'UserData');
    if ~isempty(ud) && isfield(ud,'line')
        set(ud.line, 'Color', colors(mod(muidx, n) + 1, :));
    end
    if ~isempty(ud) && isfield(ud,'anim')
        set(ud.anim, 'Color', colors(mod(muidx, n) + 1, :));
    end
end
grid on;
legend(legends, 'Location', 'Best', 'Interpreter', 'latex')
exportgraphics(gca, sprintf('System1D(u0=%.1f, v0=%.1f).png', u0, v0), 'Resolution', 300);