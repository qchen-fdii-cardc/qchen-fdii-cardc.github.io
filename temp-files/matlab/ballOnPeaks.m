function ballOnPeaks(n, visible, fn)

    arguments
        n (1, 1) int32 {mustBeNumeric, mustBePositive} = 10
        visible string {validatestring(visible, ["on", "off"])} = "on"
        fn {mustBeTextScalar} = "gridVrand.gif"
    end

    % Fix gif extension for exportgraphics
    if ~endsWith(fn, ".gif")
        fn = sprintf("%s.gif", fn);
    end

    [sx, sy, sz] = peaks(n);
    pointCount = n * n;

    szMax = max(sz(:));

    xmin = min(sx, [], 'all');
    xmax = max(sx, [], 'all');
    ymin = min(sy, [], 'all');
    ymax = max(sy, [], 'all');

    % rand - Random number generator, uniform distribution in [0, 1]
    randX = @()(rand() * (xmax - xmin) + xmin);
    randY = @()(rand() * (ymax - ymin) + ymin);

    F = scatteredInterpolant(sx(:), sy(:), sz(:));
    func = @(x, y)F(x, y);

    h = figure(Visible = visible);

    surf(sx, sy, sz, EdgeColor = "none", FaceAlpha = 0.75);
    set(gca, xlim = [-3, 3], ylim = [-3, 3], zlim = [-12, 12]);
    view(-45, 30);
    axis off
    grid off
    box off
    hold on

    x0 = randX();
    y0 = randY();
    z0 = func(x0, y0);

    m = plot3(x0, y0, z0, "o", ...
        "MarkerFaceColor", "red", ...
        "MarkerSize", 5);

    mBest = plot3(x0, y0, z0, "v", ...
        "MarkerFaceColor", "green", ...
        "MarkerSize", 8);

    if exist(fn, 'file')
        delete(fn);
    end

    title(sprintf("Walk count %3d/%3d - Highest position: %.2f/%.2f", 1, pointCount, z0, szMax));

    exportgraphics(gca, fn, Resolution = 100, Append = true);

    for t = 1:pointCount - 1
        m.XData = randX();
        m.YData = randY();
        m.ZData = func(m.XData, m.YData);

        if mBest.ZData < m.ZData
            mBest.XData = m.XData;
            mBest.YData = m.YData;
            mBest.ZData = m.ZData;
        end

        % set(gca, xlim = [-3, 3], ylim = [-3, 3], zlim = [-12, 12]);
        title(sprintf("Walk count %3d/%3d - Highest position: %.2f/%.2f", t + 1, pointCount, mBest.ZData, szMax));
        % view(-45, 30);

        drawnow
        exportgraphics(gca, fn, Resolution = 100, Append = true);
    end

    close(h);
