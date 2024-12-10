T = 0.05; % [s] Filter sample time
timeVector = 0:T:5;
[t, xTrue] = ode45(@vdp1, timeVector, [2; 0]);

figure;
plot(t, xTrue, 'LineWidth', 2);
xlabel('Time [s]');
ylabel('States');
legend('x_1', 'x_2', 'Location', 'Best');
title('Van der Pol Oscillator');
grid on;

exportgraphics(gcf, '../matlab-img/vdp1Ode.png', 'Resolution', 100);
