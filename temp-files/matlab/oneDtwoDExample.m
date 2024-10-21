x = 0:0.1:10;

y = sin(x) - cos(2 * x);

plot(x, y, LineWidth = 2);

title("Plot of sin(x) - cos(2x)");

exportgraphics(gcf, "oneDimExample.png", Resolution = 100);
