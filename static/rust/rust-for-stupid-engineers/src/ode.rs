use std::ops::Add;

pub trait OdeFunc {
    fn func(&self, t: f64, x: &Vec<f64>, dx: &mut Vec<f64>);
    fn dimension(&self) -> usize;
}

pub trait Trajectory {
    fn trajectory(&self) -> Vec<(f64, Vec<f64>)>;
}

pub fn rk6<F: OdeFunc>(
    ode_func: &F,
    t0: f64,
    t1: f64,
    x0: Vec<f64>,
    h: f64,
) -> Vec<(f64, Vec<f64>)> {
    let mut t = t0;
    let mut x = x0.clone();
    let mut trajectory = vec![(t, x.clone())];
    let n = ode_func.dimension();
    let mut k1 = vec![0.0; n];
    let mut k2 = vec![0.0; n];
    let mut k3 = vec![0.0; n];
    let mut k4 = vec![0.0; n];
    let mut k5 = vec![0.0; n];
    let mut k6 = vec![0.0; n];
    let mut dx = vec![0.0; n];

    while t < t1 {
        let h_actual = if t + h > t1 { t1 - t } else { h };

        // 计算 Runge-Kutta 的系数
        ode_func.func(t, &x, &mut k1);
        for i in 0..n {
            dx[i] = x[i] + h_actual * k1[i] / 4.0;
        }
        ode_func.func(t + h_actual / 4.0, &dx, &mut k2);
        for i in 0..n {
            dx[i] = x[i] + h_actual * (3.0 * k1[i] + 9.0 * k2[i]) / 32.0;
        }
        ode_func.func(t + 3.0 * h_actual / 8.0, &dx, &mut k3);
        for i in 0..n {
            dx[i] = x[i] + h_actual * (1932.0 * k1[i] - 7200.0 * k2[i] + 7296.0 * k3[i]) / 2197.0;
        }
        ode_func.func(t + 12.0 * h_actual / 13.0, &dx, &mut k4);
        for i in 0..n {
            dx[i] = x[i]
                + h_actual
                    * (439.0 * k1[i] / 216.0 - 8.0 * k2[i] + 3680.0 * k3[i] / 513.0
                        - 845.0 * k4[i] / 4104.0);
        }
        ode_func.func(t + h_actual, &dx, &mut k5);
        for i in 0..n {
            dx[i] = x[i]
                + h_actual
                    * (-8.0 * k1[i] / 27.0 + 2.0 * k2[i] - 3544.0 * k3[i] / 2565.0
                        + 1859.0 * k4[i] / 4104.0
                        - 11.0 * k5[i] / 40.0);
        }
        ode_func.func(t + h_actual / 2.0, &dx, &mut k6);

        // 更新状态
        for i in 0..n {
            x[i] += h_actual
                * (16.0 * k1[i] / 135.0 + 6656.0 * k3[i] / 12825.0 + 28561.0 * k4[i] / 56430.0
                    - 9.0 * k5[i] / 50.0
                    + 2.0 * k6[i] / 55.0);
        }
        t += h_actual;
        trajectory.push((t, x.clone()));
    }

    trajectory
}
