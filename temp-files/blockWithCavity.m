function gg = blockWithCavity

rect1 = [3 4 -0.5 0.5 0.5 -0.5 0.8 0.8 -0.8 -0.8];
rect2 = [3 4 -0.1 0.1 0.1 -0.1 0.4 0.4 -0.4 -0.4];
gd = [rect1', rect2'];
sf = 'R1 - R2';
ns = char('R1', 'R2')';

gg = decsg(gd, sf, ns);