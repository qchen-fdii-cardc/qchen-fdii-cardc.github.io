% 猜测初始状态
initialStateGuess = [2; 0]; % xhat[k|k-1]
% 构造 UKF
ukf = unscentedKalmanFilter( ...
    @vdpStateFcn, ... % State transition function
    @vdpMeasurementNonAdditiveNoiseFcn, ... % Measurement function
    initialStateGuess, ...
    'HasAdditiveMeasurementNoise', false);

% 设置 UKF 参数
ukf.MeasurementNoise = R;
ukf.ProcessNoise = diag([0.02 0.1]);

% 初始化数组存储
Nsteps = numel(yMeas); % Number of time steps
xCorrectedUKF = zeros(Nsteps, 2); % Corrected state estimates
PCorrected = zeros(Nsteps, 2, 2); % Corrected state estimation error covariances
e = zeros(Nsteps, 1); % Residuals (or innovations)

for k = 1:Nsteps % k时刻
    % 新息更新
    e(k) = yMeas(k) - vdpMeasurementFcn(ukf.State); % ukf.State <- x[k|k-1]
    % 新的输入测量更新状态估计
    [xCorrectedUKF(k, :), PCorrected(k, :, :)] = correct(ukf, yMeas(k)); % ukf.State <- x[k|k]
    % 预测下一个状态
    predict(ukf);
end

% 绘制结果
figure();
h = plot(timeVector, xTrue(:, 1), timeVector, xCorrectedUKF(:, 1), timeVector, yMeas(:), 'LineWidth', 0.5);
set(h(2), 'LineWidth', 2);
set(h(1), 'LineWidth', 2);

legend('True', 'UKF estimate', 'Measured', 'Location', 'Best');
xlabel('Time [s]');
ylabel('State: x_1');
title('Van der Pol Oscillator State Estimation with UKF');

exportgraphics(gcf, '../matlab-img/vdp1Ukf.png', 'Resolution', 100);

function x = vdpStateFcn(x)
    dt = 0.05; % [s] Sample time
    x = x + vdpStateFcnContinuous(x) * dt;
end

function dxdt = vdpStateFcnContinuous(x)
    %取 mu = 1的 van der Pol ODE
    dxdt = [x(2); (1 - x(1) ^ 2) * x(2) - x(1)];
end

function yk = vdpMeasurementNonAdditiveNoiseFcn(xk, vk)
    yk = xk(1) * (1 + vk);
end
