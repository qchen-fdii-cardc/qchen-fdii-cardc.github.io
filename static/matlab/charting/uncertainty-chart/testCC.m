%% plot confidence
x = 1:0.2:10;
y = besselj(0, x);
s = ConfidenceChart('XData', x , 'YData', y, 'YSigma', 0.15);

%% update data
s.YSigma = abs(y) * 0.15;


%% update chart title/labels
title(s, "Uncertainty Plot")
xlabel(s, "Time(s)")
ylabel(s, "Response")

%% export png
exportgraphics(gcf, "cc2.png", Resolution=100)

%% update data 2
s.YSigma = abs(randn(size(y))) * 0.2 + 0.1;
s.Color = [1 0 1];
exportgraphics(gcf, "cc3.png", Resolution=100)