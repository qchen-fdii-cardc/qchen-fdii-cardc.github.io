fprintf("Model and mesh...\n")
model = createpde;
geometryFromEdges(model, @cardg);
generateMesh(model, "Hmax", 0.1);
figure(1)
clf;
pdegplot(model, 'EdgeLabels', 'on')
hold on
pdemesh(model)
exportgraphics(gca, 'MeshWithEdgeLabels2.png');

fprintf("Setting up the PDE...\n");
syms u(t, x, y)
pdeeq = diff(u, t, t) - laplacian(u, [x, y]);


coeffs = pdeCoefficients(pdeeq, u)

m = @(location, state) (location.y < 0)* 0.5 + (location.y >= 0) * 1.0;

specifyCoefficients(model, 'm', m, 'd', coeffs.d, 'c', coeffs.c, 'a', coeffs.a, 'f', coeffs.f);

fprintf("Setting up the boundary conditions...\n");
setInitialConditions(model, 0, 0);
% setInitialConditions(model, 1, 0, 'Edge', 2:3)
applyBoundaryCondition(model, 'dirichlet', 'Edge', [2, 3], 'u', 1);

fprintf("Solving the PDE...\n");
results = solvepde(model, linspace(0, 10, 50));
fprintf("Done\n");

%% Visualize the solution
figure(3);

fnk = @(k)sprintf("./wave-%d.gif", k);
k = 1;
fn = fnk(k);

while exist(fn, 'file')
    k = k + 1;
    fn = fnk(k);
    % delete(fn);
end

fprintf("Visualized to file %s...\n", fn)

for idx = 1:50
    pdeplot(model, 'XYData', results.NodalSolution(:, idx), 'ColorMap', hsv(40));
    title(['t = ', num2str(results.SolutionTimes(idx))]);
    axis equal
    axis off
    colorbar off
    clim([-1, 3])
    exportgraphics(gca, fn, "append", true);
end