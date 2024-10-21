function q = qFcn(x, s)
    alpha = 1.44e-5;
    Kd = 51;
    rhod = 7100;
    cpd = Kd / rhod / alpha;

    alphp = 1.46e-5;
    Kp = 34.3;
    rhop = 4700;
    cpp = Kp / rhop / alphp;

    f = 0.5; % 摩擦系数
    omega0 = 88.464; %初始角速度
    ts = 3.96; % 终止时间
    p0 = 1.47e6 * (64.5/360); % 接触面只占了整个圆圈的64.5°

    omegat = omega0 * (1 - s.time / ts);

    eta = sqrt(Kd * rhod * cpd) / (sqrt(Kd * rhod * cpd) + sqrt(Kp * rhop * cpp));
    q = (eta) * f * omegat * x.x * p0;
end
