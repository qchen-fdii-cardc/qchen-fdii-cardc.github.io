+++
title = '001 Classic Kalman经典卡尔曼滤波（线性系统估计）'
date = 2025-02-12T09:09:02+08:00
draft = true
mathjax = false
categories = ['pe']
tags = ['pe']
toc = true
tocBorder = true
+++

## 经典卡尔曼滤波

### 有噪声的线性系统估计

对于一个线性系统，我们可以通过观测值来估计系统的状态。然而，由于观测噪声的存在，我们无法直接从观测值中准确地得到系统状态。卡尔曼滤波是一种用于从噪声数据中估计系统状态的递归算法，它通过不断更新状态估计和协方差矩阵来提高估计的准确性。

动力系统的方程通常可以表示为状态空间模型：

$$
\begin{aligned}
\mathbf{x}_k &= A \mathbf{x}_{k-1} + B \mathbf{u}_{k-1} + \mathbf{w}_{k-1} \\
\mathbf{y}_k &= C \mathbf{x}_k + D \mathbf{u}_k + \mathbf{v}_k
\end{aligned}
$$


### 经典卡尔曼滤波的四种解释及其符号表示

卡尔曼滤波是一种用于从噪声数据中估计系统状态的递归算法。以下是其在不同视角下的四种解释：

1. **数学递推方程**
   卡尔曼滤波通过递推公式更新状态和观测值，主要包括预测和更新步骤：
   - 状态预测：$\hat{x}_{k|k-1} = A \hat{x}_{k-1|k-1} + B u_{k}$
   - 观测预测：$\hat{y}_k = C \hat{x}_{k|k-1} + D v_k$
   - 计算增益：$K_k = P_{k|k-1} C^T (C P_{k|k-1} C^T + R)^{-1}$
   - 更新状态：$\hat{x}_{k|k} = \hat{x}_{k|k-1} + K_k (y_k - \hat{y}_k)$
   - 更新协方差：$P_{k|k} = (I - K_k C) P_{k|k-1}$

2. **最优线性滤波器**
   在最小均方误差准则下，卡尔曼滤波设计的是最优线性滤波器，用于估计系统状态：
   - 状态空间模型：$\mathbf{x}_k = A \mathbf{x}_{k-1} + B u_{k-1} + w_{k-1}$
   - $\mathbf{y}_k = C \mathbf{x}_k + D u_k + v_k$
   - 最小化估计误差：$\mathbb{E}[(\mathbf{x}_k - \hat{\mathbf{x}}_k)^2]$

3. **贝叶斯框架下的状态估计**
   在贝叶斯概率框架中，卡尔曼滤波通过不断更新先验概率分布得到后验概率：
   - 先验分布：$p(\mathbf{x}_k | \mathbf{y}_{1:k-1})$
   - 后验分布：$p(\mathbf{x}_k | \mathbf{y}_{1:k}) \propto p(\mathbf{y}_k | \mathbf{x}_k) p(\mathbf{x}_k |
\mathbf{y}_{1:k-1})$
   - 使用高斯分布：$\mathcal{N}(\mu, \Sigma)$

4. **系统论中的反馈机制**
   作为控制系统的一部分，卡尔曼滤波提供状态估计用于反馈控制：
   - 状态估计器设计：$\hat{\mathbf{x}}_k = A \hat{\mathbf{x}}_{k-1} + L (y_k - C \hat{\mathbf{x}}_{k-1})$
   - 确保稳定性：极点配置在左半平面

### 符号表
| 符号     | 含义           |
| -------- | -------------- |
| $x$      | 状态向量       |
| $y$      | 观测向量       |
| $v$      | 观测噪声       |
| $w$      | 过程噪声       |
| $A$      | 状态转移矩阵   |
| $C$      | 观测矩阵       |
| $B, D$   | 控制和输入矩阵 |
| $K$      | 卡尔曼增益     |
| $P$      | 协方差矩阵     |
| $\mu$    | 均值向量       |
| $\Sigma$ | 协方差矩阵     |

这些解释展示了卡尔曼滤波在数学、信号处理、统计学和控制系统中的广泛应用。


以下是一个基于经典卡尔曼滤波算法的手动实现，仅使用基础的线性代数计算工具（如矩阵乘法、逆矩阵等），不依赖 MATLAB 的高
级函数或工具箱。

### 经典卡尔曼滤波器的基本步骤
1. **状态预测**：根据状态转移矩阵和上一步的状态估计，预测当前步的状态。
2. **观测预测**：将状态预测转换为观测空间，并考虑观测噪声。
3. **计算卡尔曼增益**：用于调整更新的幅度。
4. **状态更新**：结合观测值和状态预测，得到更准确的状态估计。
5. **协方差更新**：更新状态估计的不确定性（协方差矩阵）。

### MATLAB 实现代码
```matlab
function [xEstimate, PEstimate] = KalmanFilter(x0, P0, A, B, C, D, Q, R, u, v)
    % 初始化
    N = length(u); % 控制输入的长度
    xEstimate = zeros(N, size(x0)); % 状态估计数组
    PEstimate = zeros(N, size(P0)); % 协方差估计数组

    % 初始状态和协方差
    x_kk = x0;
    P_kk = P0;

    for k = 1:N
        % 预测步骤（时间更新）
        % 状态预测
        x_kk_ = A * x_kk + B * u(k);

        % 观测预测
        y_k = C * x_kk_ + D * v(k); % 假设观测噪声为高斯分布

        % 协方差预测
        P_kk_ = A * P_kk * A' + Q; % 状态协方差更新

        % 计算卡尔曼增益
        S = C * P_kk_ * C' + R;
        K = P_kk_ * C' / S;

        % 更新步骤（测量更新）
        x_kk = x_kk_ + K * (y_k - C * x_kk_);
        P_kk = (eye(size(A, 2)) - K * C) * P_kk_;

        % 存储结果
        xEstimate(k,:) = x_kk';
        PEstimate(k,:) = diag(P_kk); % 协方差的对角线元素（假设为对角矩阵）
    end
end
```

### 代码说明
1. **输入参数**：
   - `x0`: 初始状态向量。
   - `P0`: 初始协方差矩阵。
   - `A`: 状态转移矩阵。
   - `B`: 控制输入矩阵（如果无控制输入，设为零矩阵）。
   - `C`: 观测矩阵。
   - `D`: 输入到观测噪声的传递矩阵（如果无外部噪声影响，设为零矩阵）。
   - `Q`: 过程噪声协方差矩阵。
   - `R`: 测量噪声协方差矩阵。
   - `u`: 控制输入向量（如果无控制输入，设为零向量）。
   - `v`: 测量噪声向量。

2. **输出参数**：
   - `x Estimate`: 状态估计数组。
   - `P Estimate`: 协方差估计数组。

3. **代码步骤**：
   - 初始化状态和协方差。
   - 对每一时刻进行预测（时间更新）和更新（测量更新）。
   - 计算卡尔曼增益并更新状态和协方差矩阵。

### 示例用法
```matlab
% 生成示例数据
A = [1 1; 0 1]; % 状态转移矩阵
B = zeros(2,1); % 无控制输入
C = [1 0; 0 1]; % 完全可观测系统
D = zeros(2,1);
Q = eye(2) * 0.1; % 过程噪声协方差
R = eye(2) * 0.1; % 测量噪声协方差

% 初始状态和协方差
x0 = [0; 0];
P0 = eye(2);

% 生成 true signal（例如，斜坡信号）
N = 50;
u_true = (1:N)' * 0.1;

% 加入过程噪声
v_process = sqrt(Q) * randn(N,1);
x_true = zeros(N,2);
x_true(1,:) = [0; 0];
for k = 2:N
    x_true(k,:) = A * x_true(k-1,:) + B * u_true(k-1) + v_process(k,:);
end

% 加入测量噪声
v_measure = sqrt(R) * randn(N,2);
y = C * x_true + D * v_measure;

% 调用卡尔曼滤波器
[x_Estimate, P_Estimate] = KalmanFilter(x0, P0, A, B, C, D, Q, R, zeros(N,1), v_measure);

% 可视化结果
figure;
plot(y(1,:), 'b-o', x_true(:,1), 'r--', x_Estimate(:,1), 'g-*');
title('卡尔曼滤波器估计示例（第一状态）');
legend({'测量值', '真实信号', '滤波估计'}, 'Location', 'SouthEast');

figure;
plot(y(2,:), 'b-o', x_true(:,2), 'r--', x_Estimate(:,2), 'g-*');
title('卡尔曼滤波器估计示例（第二状态）');
legend({'测量值', '真实信号', '滤波估计'}, 'Location', 'SouthEast');
```

