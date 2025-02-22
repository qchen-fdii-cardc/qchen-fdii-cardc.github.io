+++
title = 'Kalman Filter by Gsl用C语言做一个速度跟踪的卡尔曼滤波器'
date = 2025-02-20T17:31:52+08:00
draft = false
mathjax = true
categories = ['c']
tags = ['c', 'kalman filter', 'gsl']
toc = true
tocBorder = true
+++

## 系统模型

实现的卡尔曼滤波器用于跟踪一维空间中物体的位置和速度。系统动力学模型用线性系统来描述如下:

### 连续时间系统方程
$$
\dot{\mathbf{x}} = A_c \mathbf{x} + \mathbf{w}
$$

其中，状态向量 $\mathbf{x}$ 包含位置 $r$ 和速度 $v$，过程噪声 $\mathbf{w}$ 是均值为零的高斯白噪声，其强度矩阵为 $Q_c$。

观测过程可以表示为：
$$
z= H \mathbf{x} + \mathbf{v}
$$

其中，观测矩阵 $H$ 提取可测量的状态（位置），观测噪声 $\mathbf{v}$ 是均值为零的高斯白噪声，其协方差矩阵为 $R$。

### 离散时间系统方程
连续系统离散化后得到：
$$
\mathbf{x}_{k+1} = A\mathbf{x}_k + \mathbf{\nu}_k
$$

其中：
- $A = e^{A_c dt}$ 是离散状态转移矩阵
- $\mathbf{\nu}_k$ 是离散过程噪声，其协方差矩阵为 $Q$
- $dt$ 是采样时间间隔

### 状态向量
状态向量包含：
$$
\mathbf{x} = \begin{bmatrix} 
r \\\\
v
\end{bmatrix}
$$

### 系统矩阵
连续时间系统矩阵：
$$
A_c = \begin{bmatrix}
0 & 1 \\\\
0 & 0
\end{bmatrix}
$$

离散化后的状态转移矩阵：
$$
A = \begin{bmatrix}
1 & dt \\\\
0 & 1
\end{bmatrix}
$$

### 过程噪声协方差矩阵
连续时间过程噪声强度矩阵（假设加速度扰动）：
$$
Q_c = \begin{bmatrix}
0 & 0 \\\\
0 & q
\end{bmatrix}
$$
其中 $q$ 是加速度噪声的强度（单位：$m^2/s^4$）。

离散化后的过程噪声协方差矩阵：
$$
Q = \begin{bmatrix}
\frac{dt^4}{4}q & \frac{dt^3}{2}q \\\\
\frac{dt^3}{2}q & dt^2q
\end{bmatrix}
$$

这在速度跟踪问题中的加速度噪声假设中的经典的结果，不在这里详细解释。


1. **物理意义**：
   - $q$ 是连续时间加速度噪声强度（单位：$m^2/s^4$）
   - 通过系统响应积分得到离散时间协方差矩阵

2. **矩阵元素解释**：
   - $Q_{11}$：位置误差的方差，由加速度噪声经两次积分得到
   - $Q_{22}$：速度误差的方差，由加速度噪声经一次积分得到
   - $Q_{12}, Q_{21}$：位置和速度误差的协方差


### 测量模型
离散测量方程：
$$
z_k = H\mathbf{x}_k + \mathbf{v}_k
$$

测量矩阵：
$$
H = \begin{bmatrix}
1 & 0
\end{bmatrix}
$$

测量噪声协方差：
$$
R = [r]
$$
其中 $r$ 是位置测量噪声的方差（单位：$m^2$）

## 卡尔曼滤波器算法

该算法包含两个主要步骤：

### 预测步骤

状态预测：

$$
   \hat{x}\_{k|k-1} = A\hat{x}\_{k-1|k-1}
$$

协方差预测：

$$
P_{k|k-1} = AP_{k-1|k-1}A^T + Q
$$

### 更新步骤
1. 新息协方差：
$$
S = HP_{k|k-1}H^T + R
$$

2. 卡尔曼增益：
$$
K = P_{k|k-1}H^T S^{-1}
$$

3. 状态更新：
$$
\hat{x}\_{k|k} = \hat{x}\_{k|k-1} + K(z_k - H\hat{x}\_{k|k-1})
$$

4. 协方差更新：
$$
P_{k|k} = (I - KH)P_{k|k-1}
$$

## 实现细节

### 矩阵运算
程序使用 GSL（GNU 科学库）进行矩阵运算：
- `gsl_blas_dgemm`：矩阵乘法
- `gsl_matrix_add`：矩阵加法
- `gsl_matrix_sub`：矩阵减法
- `gsl_matrix_scale`：标量乘法

### 内存管理
实现过程中谨慎管理内存：
1. 在初始化期间分配矩阵
2. 使用临时矩阵进行中间计算
3. 适时释放所有矩阵

### 仿真
主程序模拟：
1. 匀速运动的真实轨迹
2. 使用高斯噪声的带噪声测量
3. 对这些测量进行滤波以估计真实状态

## 噪声参数

滤波器使用两个主要噪声参数：
- 过程噪声 (σ_p^2 = 0.01)：模拟运动中的不确定性
- 测量噪声 (σ_m^2 = 0.01)：模拟传感器的不确定性

## 性能

滤波器输出：
1. 时间
2. 真实位置
3. 测量位置（带噪声）
4. 估计位置
5. 估计速度

随着处理更多的测量值，估计值应该收敛到真实值，滤波器能有效降低测量噪声同时跟踪真实状态。

## 代码实现与结果可视化

```c
{{% codesnap "static/cpp/kalman-filter/kalman.c" %}}
```

编译：

```bash
gcc kalman.c -o kalman -lgsl -lgslcblas -lm
```

需要系统中安装了 gsl 库。安装方法：

```bash
sudo apt-get install libgsl-dev
```

运行：

```bash
./kalman > kalman_data.txt
```

就能看到一个文本文件 `kalman_data.txt`，里面记录了时间、真实位置、真实速度、测量位置、估计位置、估计速度。



## 结果可视化
很无聊的用Python做了一个可视化，直接从c代码生成可执行文件，然后捕获输出画图。

```python
{{% codesnap "static/cpp/kalman-filter/plot_kalman.py" %}}
```


![kalman_position](/cpp/kalman-filter/kalman_filter_position.png)

可以看到，我们从速度0开始，很快就追踪到速度1附近，然后速度估计基本就稳定在1附近了。

![kalman_velocity](/cpp/kalman-filter/kalman_filter_velocity.png)





