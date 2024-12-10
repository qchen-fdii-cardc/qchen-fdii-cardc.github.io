R = 0.2;

rng(1); % For reproducibility
yTrue = xTrue(:, 1);
yMeas = yTrue .* (1 + sqrt(R) * randn(size(yTrue)));

figure;
plot(t, yTrue, 'LineWidth', 2);
hold on;
plot(t, yMeas, 'LineWidth', 2);
xlabel('Time [s]');
ylabel('State: x_1');
legend('True', 'Measured', 'Location', 'Best');
title('Van der Pol Oscillator Observations of x_1');

exportgraphics(gcf, '../matlab-img/vdp1Observ.png', 'Resolution', 100);
