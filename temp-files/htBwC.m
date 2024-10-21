

%%
g = blockWithCavity;
model = femodel(AnalysisType="thermalTransient",...
    Geometry=g);

h = figure(1);
pdegplot(model,EdgeLabels="on");
xlim([-0.6,0.6])
ylim([-1,1])



%%

% 设定材料
alphad = 1.44e-5; % 热扩散系数
Kd = 51;
rhod = 7100;
cpd = Kd / rhod / alphad;

model.MaterialProperties = ...
            materialProperties(ThermalConductivity=1, ...
                               MassDensity=1, ...
                               SpecificHeat=1);

%%

model.EdgeBC(6) = edgeBC(Temperature=100);
model.EdgeLoad(1) = edgeLoad(Heat=-10);

model.FaceIC = faceIC(Temperature=-10);

%%
model = generateMesh(model);

figure(2);
pdemesh(model);
title("Mesh with Quadratic Triangular Elements")
xlim([-0.6,0.6])
ylim([-1,1])
%%

tlist = 0:.1:5.0;
results = solve(model,tlist)


%%
[qx,qy] = evaluateHeatFlux(results);

fn = 'C:\PARA\0_Projects\tech-blog\qchen-fdii-cardc.github.io\static\matlab-img\cavity.gif';
if exist(fn, 'file')
    delete(fn);
end

figure(3)

for i = 2:size(results.Temperature, 2)
    c = pdeplot(results.Mesh,XYData=results.Temperature(:,i), ...
                         Contour="on",...
                         FlowData=[qx(:,i),qy(:,i)], ...
                         ColorMap="hot");
    xlim([-0.6,0.6])
    ylim([-1,1])
    axis equal
    title(sprintf("t = %4.2f", results.SolutionTimes(i)))
    exportgraphics(gca, fn, Resolution=100, Append=true);    
end
