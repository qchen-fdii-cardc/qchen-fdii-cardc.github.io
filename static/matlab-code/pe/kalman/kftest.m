% 生成示例数据
A = [1 1; 0 1]; % 状态转移矩阵
B = zeros(2,1); % 无控制输入
C = [1 0; 0 1]; % 完全可观测系统
D = zeros(2,2);
Q = eye(2) * 0.1; % 过程噪声协方差
R = eye(2) * 0.1; % 测量噪声协方差

% 初始状态和协方差
x0 = [0; 0];
P0 = eye(2);

% 生成 true signal（例如，斜坡信号）
N = 50;
u_true = (1:N)' * 0.1;

% 加入过程噪声
v_process = repmat(sqrt(diag(Q)), 1, N) .* randn(2,N);
x_true = zeros(2, N);
x_true(:,1) = [0; 0];
for k = 2:N
    x_true(:,k) = A * x_true(:,k-1) + B * u_true(k-1) + v_process(:, k);
end

% 加入测量噪声
v_measure = repmat(sqrt(diag(R)), 1, N) .* randn(2,N);
y = C * x_true + D * v_measure;

% 调用卡尔曼滤波器
[x_Estimate, P_Estimate] = KalmanFilter(x0, P0, A, B, C, D, Q, R, zeros(N,1), v_measure);

% 可视化结果
figure;
plot(1:N, y(1,:), 'b-o',1:N,  x_true(1,:), 'r--',1:N,  x_Estimate(1,:), 'g-*');
title('卡尔曼滤波器估计示例（第一状态）');
legend({'测量值', '真实信号', '滤波估计'}, 'Location', 'SouthEast');

figure;
plot(1:N, y(2,:), 'b-o', 1:N, x_true(2,:), 'r--',1:N,  x_Estimate(2,:), 'g-*');
title('卡尔曼滤波器估计示例（第二状态）');
legend({'测量值', '真实信号', '滤波估计'}, 'Location', 'SouthEast');