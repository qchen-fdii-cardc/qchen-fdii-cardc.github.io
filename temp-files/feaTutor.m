R1 = [3, 4, [66, 76.5, 76.5, 66, -5.5, -5.5, 0, 0] / 1000]';
R2 = [3, 4, [76.5, 113.5, 113.5, 76.5, -5.5, -5.5, 0, 0] / 1000]';

gdm = [R1 R2];

ns = char('R1', 'R2');
g = decsg(gdm, "R1 + R2", ns');

fig = figure;
pdegplot(g, EdgeLabels = 'on', FaceLabels = 'on');

% exportgraphics(fig, 'feaTutor.png', Resolution = 600);

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

fig2 = figure;
plot(tlist, iTRd)
hold on
plot(tlist, iTrp)
plot(tlist, iTrd)
title("Temperature Variation with Time at Key Radial Locations")
legend("R_d", "r_p", "r_d")
xlabel("t, s")
ylabel("T,^{\circ}C")

exportgraphics(fig2, 'thermalResponse.png', Resolution = 600)

%% Stress Analysis

% 改变分析类型

model.AnalysisType = 'structuralStatic';

% 设定材料

model.MaterialProperties = ...
    materialProperties(YoungsModulus = 99.97E9, ...
    PoissonsRatio = 0.29, ...
    CTE = 1.08E-5);

% 固定模型，避免刚体运动

model.EdgeBC([3, 4]) = edgeBC(YDisplacement = 0); % 固定刹车片中心对称面

% 设定参考温度，对应于0热应力的温度
model.ReferenceTemperature = 20;

% 根据热分析结果 $Rt$ 来设定热载荷。求解时间与热分析相同。 对于每一个时间点， 求解对应的结构静分析问题，并绘制温度分布、周向应力， hoop应力， von Mises应力。

for n = 2:numel(Rt.SolutionTimes)
    Rt_step = filterByIndex(Rt, n);
    model.FaceLoad = faceLoad(Temperature = Rt_step);
    R = solve(model);
    plotResults(R, Rt, n);
end
