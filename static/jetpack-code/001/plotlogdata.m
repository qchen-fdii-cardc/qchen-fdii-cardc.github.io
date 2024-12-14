ypr = readtable('20241214085714.csv');
plot(ypr.Time, ypr.Yaw, 'o-', ...
    ypr.Time, ypr.Pitch, 'x-',...
    ypr.Time, ypr.Roll, 's-', 'LineWidth', 2)
legend({'Yaw', 'Pitch', 'Roll'}, 'Location', 'Best');
xlabel('Time(s)');ylabel('Angle(^o)');
exportgraphics(gca, '../../jetpack-img/001/log.png');