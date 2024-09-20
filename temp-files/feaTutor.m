R1 = [3, 4, [66, 76.5, 76.5, 66, -5.5, -5.5, 0, 0] / 1000]';
R2 = [3, 4, [76.5, 113.5, 113.5, 76.5, -5.5, -5.5, 0, 0] / 1000]';

gdm = [R1 R2];

ns = char('R1', 'R2');
g = decsg(gdm, "R1 + R2", ns');

fig = figure;
pdegplot(g, EdgeLabels = 'on', FaceLabels = 'on');

exportgraphics(fig, 'feaTutor.png', Resolution = 600);

% create model
model = femodel(AnalysisType = 'thermalTransient', Geometry = g);
model.PlanarType = 'axisymmetric';

% 设定几何

model.Geometry = geometryFromEdges(g);

% 产生网格
model = generateMesh(model, Hmax = 0.5e-4, ...
    GeometricOrder = "linear");

% 设定材料
alphad = 1.44e-5; % 热扩散系数
Kd = 51;
rhod = 7100;
cpd = Kd / rhod / alphad;

model.MaterialProperties = ...
    materialProperties(ThermalConductivity = Kd, ...
    MassDensity = rhod, ...
    SpecificHeat = cpd);

% 热流边界层条件

model.EdgeLoad(6) = edgeLoad(Heat = @qFcn);

% 设定初始条件
model.FaceIC = faceIC(Temperature = 20);

% 求解时间
tlist = [0 0.1 0.2 1.0 2.0 3.0 3.96];
Rt = solve(model, tlist);

% 绘图
iTRd = interpolateTemperature(Rt, [0.1135; 0], 1:numel(Rt.SolutionTimes));
iTrp = interpolateTemperature(Rt, [0.0765; 0], 1:numel(Rt.SolutionTimes));
iTrd = interpolateTemperature(Rt, [0.066; 0], 1:numel(Rt.SolutionTimes));

figure
plot(tlist, iTRd)
hold on
plot(tlist, iTrp)
plot(tlist, iTrd)
title("Temperature Variation with Time at Key Radial Locations")
legend("R_d", "r_p", "r_d")
xlabel("t, s")
ylabel("T,^{\circ}C")

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

function plotResults(R, Rt, tID)
    figure
    subplot(2, 2, 1)
    pdeplot(Rt.Mesh, XYData = Rt.Temperature(:, tID), ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Temperature'; ...
               ['max = ' num2str(max(Rt.Temperature(:, tID))) '^{\circ}C']}, ...
        FontSize = 10)

    subplot(2, 2, 2)
    pdeplot(R.Mesh, XYData = R.Stress.srr, ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Radial Stress'; ...
               ['min = ' num2str(min(R.Stress.srr) / 1E6, '%3.2f') ' MPa'];
           ['max = ' num2str(max(R.Stress.srr) / 1E6, '%3.2f') ' MPa']}, ...
        FontSize = 10)

    subplot(2, 2, 3)
    pdeplot(R.Mesh, XYData = R.Stress.sh, ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Hoop Stress'; ...
               ['min = ' num2str(min(R.Stress.sh) / 1E6, '%3.2f') ' MPa'];
           ['max = ' num2str(max(R.Stress.sh) / 1E6, '%3.2f') ' MPa']}, ...
        FontSize = 10)

    subplot(2, 2, 4)
    pdeplot(R.Mesh, XYData = R.VonMisesStress, ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Von Mises Stress'; ...
               ['max = ' num2str(max(R.VonMisesStress) / 1E6, '%3.2f') ' MPa']}, ...
        FontSize = 10)

    sgtitle(['Time = ' num2str(Rt.SolutionTimes(tID)) ' s'], FontWeight = "bold")
end
