function ballOnPeaksContour(n, visible, fn)

    arguments
        n (1, 1) int32 {mustBeNumeric, mustBePositive} = 10
        visible string {validatestring(visible, ["on", "off"])} = "on"
        fn {mustBeTextScalar} = "gridVrandContour.gif"
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

    contour(sx, sy, sz, 20);
    set(gca, xlim = [-4, 4], ylim = [-4, 4]);
    axis off
    grid off
    box off
    hold on

    x0 = randX();
    y0 = randY();
    z0 = func(x0, y0);

    m = plot(x0, y0, "o", ...
        "MarkerFaceColor", "red", ...
        "MarkerSize", 5);

    mBest = plot(x0, y0, "v", ...
        "MarkerFaceColor", "green", ...
        "MarkerSize", 8);
    mBestZData = z0;

    if exist(fn, 'file')
        delete(fn);
    end

    title(sprintf("Walk count %3d/%3d - Highest position: %4.2f/%4.2f", 1, pointCount, z0, szMax));

    exportgraphics(gca, fn, Resolution = 100, Append = true);

    for t = 1:pointCount - 1
        m.XData = randX();
        m.YData = randY();
        ZData = func(m.XData, m.YData);

        if mBestZData < ZData
            mBest.XData = m.XData;
            mBest.YData = m.YData;
            mBestZData = ZData;
        end

        title(sprintf("Walk count %3d/%3d - Highest position: %4.2f/%4.2f", t + 1, pointCount, mBestZData, szMax));

        drawnow
        exportgraphics(gca, fn, Resolution = 100, Append = true);
    end

    close(h);
