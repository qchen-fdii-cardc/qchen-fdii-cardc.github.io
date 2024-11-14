model = createpde();

g = importGeometry(model, "Block.stl");

figure(1);
pdegplot(g, "VertexLabels", "on", "FaceAlpha", 0.5);

generateMesh(model);
pdeplot3D(model);

VertexID = addVertex(g, "Coordinates", [20 0 50]); % Add a vertex at (20, 0, 50), and return the vertex ID = 9
figure(2);
pdegplot(g, "VertexLabels", "on", "FaceAlpha", 0.5);
generateMesh(model, "Hvertex", {9, 0.1});
pdeplot3D(model);

%%
model = createpde();

g = importGeometry(model, "Block.stl");
V = ([20 0 50; 40 0 50; 60 0 50; 80 0 50]);
VertexIDs = addVertex(g, "Coordinates", V); % Add vertices at (40, 0, 50), (60, 0, 50), and (80, 0, 50), and return the vertex IDs = 10, 11, 12

figure(1);
pdegplot(g, "VertexLabels", "on", "FaceAlpha", 0.5);
exportgraphics(gcf, '../matlab-img/origin-geometry-4p.png')

figure(2);
generateMesh(model, "Hvertex", {VertexIDs, 0.1});
pdeplot3D(model);
exportgraphics(gcf, '../matlab-img/origin-geometry-4p-meshing.png')
%%
VertexID = addVertex(g, "Coordinates", [100 0 50]); % Add a vertex at (100, 0, 50), already exist, return the vertex ID = 5
figure(4);
pdegplot(g, "VertexLabels", "on", "FaceAlpha", 0.5);

%Create the 3D geometry by extruding two adjacent rectangles
gd = [3 4 -0.15 -0.11 -0.11 -0.15 -0.015 -0.015 .015 .015;
      3 4 -0.11 0.15 0.15 -0.11 -0.015 -0.015 .015 .015]';
dl = decsg(gd);
gm = geometryFromEdges(dl);
gm = extrude(gm, 0.003);
%Create the model and add the geometry
model = createpde("structural", "modal-solid");
model.Geometry = gm;
figure(5);
pdegplot(gm, "EdgeLabels", "on", "FaceLabels", "on")

generateMesh(model, "Hedge", {14, 0.001})
pdeplot3D(model);
