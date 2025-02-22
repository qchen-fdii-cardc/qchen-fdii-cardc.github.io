#include <stdio.h>
#include <math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

// 卡尔曼滤波器结构体
typedef struct
{
    gsl_matrix *A; // 状态转移矩阵
    gsl_matrix *H; // 观测矩阵
    gsl_matrix *Q; // 过程噪声协方差
    gsl_matrix *R; // 测量噪声协方差
    gsl_matrix *P; // 估计误差协方差
    gsl_matrix *K; // 卡尔曼增益
    gsl_matrix *x; // 状态向量 [位置, 速度]
} KalmanFilter;

// 初始化卡尔曼滤波器
KalmanFilter *init_kalman_filter(double dt, double process_noise, double measurement_noise)
{
    KalmanFilter *kf = malloc(sizeof(KalmanFilter));

    // 初始化矩阵
    kf->A = gsl_matrix_alloc(2, 2);
    kf->H = gsl_matrix_alloc(1, 2);
    kf->Q = gsl_matrix_alloc(2, 2);
    kf->R = gsl_matrix_alloc(1, 1);
    kf->P = gsl_matrix_alloc(2, 2);
    kf->K = gsl_matrix_alloc(2, 1);
    kf->x = gsl_matrix_alloc(2, 1);

    // 设置状态转移矩阵 A
    gsl_matrix_set(kf->A, 0, 0, 1.0);
    gsl_matrix_set(kf->A, 0, 1, dt);
    gsl_matrix_set(kf->A, 1, 0, 0.0);
    gsl_matrix_set(kf->A, 1, 1, 1.0);

    // 设置观测矩阵 H
    gsl_matrix_set(kf->H, 0, 0, 1.0);
    gsl_matrix_set(kf->H, 0, 1, 0.0);

    // 设置过程噪声协方差 Q
    gsl_matrix_set(kf->Q, 0, 0, 0.25 * dt * dt * dt * dt * process_noise);
    gsl_matrix_set(kf->Q, 0, 1, 0.5 * dt * dt * dt * process_noise);
    gsl_matrix_set(kf->Q, 1, 0, 0.5 * dt * dt * dt * process_noise);
    gsl_matrix_set(kf->Q, 1, 1, dt * dt * process_noise);

    // 设置测量噪声协方差 R
    gsl_matrix_set(kf->R, 0, 0, measurement_noise);

    // 初始化估计误差协方差 P
    gsl_matrix_set_identity(kf->P);
    gsl_matrix_scale(kf->P, 1.0);

    // 初始化状态向量
    gsl_matrix_set_zero(kf->x);

    return kf;
}

// 预测步骤
void predict(KalmanFilter *kf)
{
    // 临时矩阵
    gsl_matrix *temp_2x2 = gsl_matrix_alloc(2, 2);
    gsl_matrix *temp_2x1 = gsl_matrix_alloc(2, 1);

    // x = A * x
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, kf->A, kf->x, 0.0, temp_2x1);
    gsl_matrix_memcpy(kf->x, temp_2x1);

    // P = A * P * A' + Q
    gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1.0, kf->A, kf->P, 0.0, temp_2x2);
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, temp_2x2, kf->A, 0.0, kf->P);
    gsl_matrix_add(kf->P, kf->Q);

    // 释放临时矩阵
    gsl_matrix_free(temp_2x2);
    gsl_matrix_free(temp_2x1);
}

// 更新步骤
void update(KalmanFilter *kf, double measurement)
{
    // 临时矩阵
    gsl_matrix *temp_1x2 = gsl_matrix_alloc(1, 2);   // For H*P
    gsl_matrix *temp_2x1 = gsl_matrix_alloc(2, 1);   // For P*H'
    gsl_matrix *temp_1x1 = gsl_matrix_alloc(1, 1);   // For H*x or innovation
    gsl_matrix *S = gsl_matrix_alloc(1, 1);          // Innovation covariance
    gsl_matrix *z = gsl_matrix_alloc(1, 1);          // Measurement
    gsl_matrix *innovation = gsl_matrix_alloc(1, 1); // z - H*x

    // 设置测量值
    gsl_matrix_set(z, 0, 0, measurement);

    // S = H * P * H' + R
    // First compute H*P (1x2 * 2x2 = 1x2)
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, kf->H, kf->P, 0.0, temp_1x2);
    // Then (H*P) * H' (1x2 * 2x1 = 1x1)
    gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1.0, temp_1x2, kf->H, 0.0, S);
    gsl_matrix_add(S, kf->R);

    // K = P * H' * S^(-1)
    // First compute P*H' (2x2 * 2x1 = 2x1)
    gsl_blas_dgemm(CblasNoTrans, CblasTrans, 1.0, kf->P, kf->H, 0.0, temp_2x1);
    // Then scale by S^(-1)
    double s_inv = 1.0 / gsl_matrix_get(S, 0, 0);
    gsl_matrix_memcpy(kf->K, temp_2x1);
    gsl_matrix_scale(kf->K, s_inv);

    // 计算创新项 (innovation = z - H*x)
    // First compute H*x (1x2 * 2x1 = 1x1)
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, kf->H, kf->x, 0.0, temp_1x1);
    gsl_matrix_memcpy(innovation, z);
    gsl_matrix_sub(innovation, temp_1x1);

    // x = x + K * innovation
    // K*innovation (2x1 * 1x1 = 2x1)
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, kf->K, innovation, 1.0, kf->x);

    // P = (I - K*H) * P
    // First compute K*H (2x1 * 1x2 = 2x2)
    gsl_matrix *temp_2x2 = gsl_matrix_alloc(2, 2);
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, -1.0, kf->K, kf->H, 0.0, temp_2x2);
    gsl_matrix_set_identity(temp_2x2); // Add identity matrix
    // Then multiply by P
    gsl_matrix *new_P = gsl_matrix_alloc(2, 2);
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, temp_2x2, kf->P, 0.0, new_P);
    gsl_matrix_memcpy(kf->P, new_P);

    // 释放临时矩阵
    gsl_matrix_free(temp_1x2);
    gsl_matrix_free(temp_2x1);
    gsl_matrix_free(temp_1x1);
    gsl_matrix_free(temp_2x2);
    gsl_matrix_free(new_P);
    gsl_matrix_free(S);
    gsl_matrix_free(z);
    gsl_matrix_free(innovation);
}

// 释放卡尔曼滤波器
void free_kalman_filter(KalmanFilter *kf)
{
    gsl_matrix_free(kf->A);
    gsl_matrix_free(kf->H);
    gsl_matrix_free(kf->Q);
    gsl_matrix_free(kf->R);
    gsl_matrix_free(kf->P);
    gsl_matrix_free(kf->K);
    gsl_matrix_free(kf->x);
    free(kf);
}

int main()
{
    // 初始化随机数生成器
    gsl_rng *rng = gsl_rng_alloc(gsl_rng_default);
    gsl_rng_set(rng, 1234);

    // 初始化卡尔曼滤波器参数
    double dt = 0.1;                 // 时间步长
    double process_noise = 0.01;     // 过程噪声
    double measurement_noise = 0.01; // 测量噪声

    // 创建卡尔曼滤波器
    KalmanFilter *kf = init_kalman_filter(dt, process_noise, measurement_noise);

    // 模拟数据
    double true_position = 0.0;
    double true_velocity = 1.0;

    // 使用固定宽度格式化输出表头
    printf("#%8s\t%8s\t%8s\t%8s\t%8s\t%8s\n",
           "Time", "True Pos", "True Vel",
           "Meas Pos",
           "Est Pos", "Est Vel");

    // 运行模拟
    for (int t = 0; t < 100; t++)
    {
        // 加速度扰动考虑
        true_velocity += gsl_ran_gaussian(rng, sqrt(process_noise)) * dt;

        // 更新真实状态
        true_position += true_velocity * dt;

        // 生成带噪声的测量值
        double measurement = true_position + gsl_ran_gaussian(rng, sqrt(measurement_noise));

        // 卡尔曼滤波
        predict(kf);
        update(kf, measurement);

        // 使用固定宽度格式化输出数据
        printf("%9.2f\t%8.4f\t%8.4f\t%8.4f\t%8.4f\t%8.4f\n",
               t * dt,
               true_position,
               true_velocity,
               measurement,
               gsl_matrix_get(kf->x, 0, 0),
               gsl_matrix_get(kf->x, 1, 0));
    }

    // 清理
    free_kalman_filter(kf);
    gsl_rng_free(rng);

    return 0;
}
