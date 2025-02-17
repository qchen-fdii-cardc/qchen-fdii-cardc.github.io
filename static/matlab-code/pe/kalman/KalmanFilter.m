function [xEstimate, PEstimate] = KalmanFilter(x0, P0, A, B, C, D, Q, R, u, v)
% 初始化
N = length(u); % 控制输入的长度
xEstimate = zeros(numel(x0), N); % 状态估计数组
PEstimate = zeros([size(P0), N]); % 协方差估计数组

% 初始状态和协方差
x_kk = x0;
P_kk = P0;

for k = 1:N
    % 预测步骤（时间更新）
    % 状态预测
    x_kk_ = A * x_kk + B * u(k);
    
    % 观测预测
    y_k = C * x_kk_ + D * v(:, k); % 假设观测噪声为高斯分布
    
    % 协方差预测
    P_kk_ = A * P_kk * A' + Q; % 状态协方差更新
    
    % 计算卡尔曼增益
    S = C * P_kk_ * C' + R;
    K = P_kk_ * C' / S;
    
    % 更新步骤（测量更新）
    x_kk = x_kk_ + K * (y_k - C * x_kk_);
    P_kk = (eye(size(A, 2)) - K * C) * P_kk_;
    
    % 存储结果
    xEstimate(:, N) = x_kk;
    PEstimate(:,:,k) = P_kk; % 协方差的对角线元素（假设为对角矩阵）
end
end