% 非均匀材料的热传导问题


model = femodel("AnalysisType","thermalTransient");

% 几何建模
length = 10;
gm = multicuboid([1, 1],[1, 1], [length * 0.5, length * 0.5], 'Zoffset', [0, 0.5*length]);

pdegplot(gm, 'CellLabels', 'on', 'FaceLabels', 'on');

view(57, 56);
exportgraphics(gcf, 'heatTransferInNonUniformPod.png');

model.Geometry = gm;

model = generateMesh(model);

pdemesh(model);

% 材料建模
material1 = materialProperties("ThermalConductivity", 1, "MassDensity", 1, "SpecificHeat", 1);
material2 = materialProperties("ThermalConductivity", 100, "MassDensity", 1, "SpecificHeat", 1);

model.MaterialProperties(1) = material1;
model.MaterialProperties(2) = material2;

model.CellIC = cellIC("Temperature", 20);

model.FaceBC(1) = faceBC("Temperature", 20);
model.FaceBC(7) = faceBC("Temperature", 0);

tspan = linspace(0, 200, 1000);
result = solve(model, tspan);

% using pdeviz build a animation of the heating process

V = pdeviz(model.Mesh, result.Temperature(:,1));
V.ColorLimits = [0, 20];
colorbar;
for ti = 1:numel(tspan)
    V.NodalData = result.Temperature(:,ti);
    title(sprintf('Time: %.2f', tspan(ti)));
    pause(0.001);
end


