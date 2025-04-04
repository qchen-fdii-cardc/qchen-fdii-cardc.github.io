ret = cq.Workplane("front").box(1,1,1);



result0 = cq.Workplane("front").circle(2.0).rect(0.5, 0.75).extrude(0.5);


arcExtrude = ...
   cq.Workplane("front")...
   .lineTo(2.0, 0)...
   .lineTo(2.0, 1.0)...
   .threePointArc([1.0, 1.5], [0.0, 1.0])...
   .close()...
   .extrude(0.25);


%%%%%%%%%%%%%%%%%%%%%%%
result1 = cq.Workplane("front").circle(...
    3.0...
)  % current point is the center of the circle, at (0, 0)
result1 = result1.center(1.5, 0.0).rect(0.5, 0.5)  % new work center is (1.5, 0.0)

result1 = result1.center(-1.5, 1.5).circle(0.25)  % new work center is (0.0, 1.5).
% The new center is specified relative to the previous center, not global coordinates!

result1 = result1.extrude(0.25);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
r = cq.Workplane("front").circle(2.0)  % make base
r = r.pushPoints(...
   matrixRow2List([1.5, 0; 0, 1.5; -1.5, 0; 0, -1.5])...
)  % now four points are on the stack
r = r.circle(0.25)  % circle will operate on all four points
result3 = r.extrude(0.125)  % make prism


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
result4 = ...
    cq.Workplane("front")...
    .box(3.0, 4.0, 0.25)...
    .pushPoints(matrixRow2List([0, 0.75; 0, -0.75]))...
    .polygon(py.int(6), 1)...
    .cutThruAll()