% 非均匀材料的热传导问题


model = femodel("AnalysisType","thermalTransient");

% 几何建模
length = 10;
gm = multicuboid(1, 1, length);

pdegplot(gm, 'CellLabels', 'on', 'FaceLabels', 'on');

view(57, 56);
exportgraphics(gcf, 'singleCell.png', 'Resolution', 100);

model.Geometry = gm;

model = generateMesh(model);

pdemesh(model);

% 材料建模
% k = @(location, state) 9.9 * location.z + 1;

model.MaterialProperties = materialProperties(...
    "ThermalConductivity", @k_func,...
    "MassDensity", 1,...
    "SpecificHeat", 1);

model.CellIC = cellIC("Temperature", 20);

model.FaceBC(1) = faceBC("Temperature", 20);
model.FaceBC(2) = faceBC("Temperature", 0);

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

exportgraphics(gcf, 'result2.png', 'Resolution', 60);

function k = k_func(location, ~)
k = 9.9 * location.z + 1;
end

