function plotResults(R, Rt, tID)
    figure
    subplot(2, 2, 1)
    pdeplot(Rt.Mesh, XYData = Rt.Temperature(:, tID), ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Temperature'; ...
               ['max = ' num2str(max(Rt.Temperature(:, tID))) '^{\circ}C']}, ...
        FontSize = 10)

    subplot(2, 2, 2)
    pdeplot(R.Mesh, XYData = R.Stress.srr, ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Radial Stress'; ...
               ['min = ' num2str(min(R.Stress.srr) / 1E6, '%3.2f') ' MPa'];
           ['max = ' num2str(max(R.Stress.srr) / 1E6, '%3.2f') ' MPa']}, ...
        FontSize = 10)

    subplot(2, 2, 3)
    pdeplot(R.Mesh, XYData = R.Stress.sh, ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Hoop Stress'; ...
               ['min = ' num2str(min(R.Stress.sh) / 1E6, '%3.2f') ' MPa'];
           ['max = ' num2str(max(R.Stress.sh) / 1E6, '%3.2f') ' MPa']}, ...
        FontSize = 10)

    subplot(2, 2, 4)
    pdeplot(R.Mesh, XYData = R.VonMisesStress, ...
        ColorMap = "jet", Contour = "on")
    xt = xticks;
    yt = yticks;
    xticklabels(100 * xt);
    yticklabels(100 * yt);
    xlabel("x,cm")
    ylabel("y,cm")
    title({'Von Mises Stress'; ...
               ['max = ' num2str(max(R.VonMisesStress) / 1E6, '%3.2f') ' MPa']}, ...
        FontSize = 10)

    sgtitle(['Time = ' num2str(Rt.SolutionTimes(tID)) ' s'], FontWeight = "bold")
end
