function fig = uiLayout()

    % Follow the convention to return the figure object
    % Since uifigure with "HandleVisibility" set to "off" is not accessible by gcf
    fig = uifigure;

    %  update window size, width x height
    fig.Position(3:4) = [800 600];
    % update widnow position with respect to the screen
    movegui(fig, 'center');

    peppers = uiimage(fig);
    peppers.ImageSource = "peppers.png";
    peppers.Position(1:2) = [10 300];

    street = uiimage(fig);
    street.ImageSource = "street1.jpg";
    street.Position(1:2) = [230 250];

    nebula = uiimage(fig);
    nebula.ImageSource = "ngc6543a.jpg";
    nebula.Position(1:2) = [150 180];

    for handle = [peppers, street, nebula]
        fprintf("Image source: %s\n", handle.ImageSource);
        handle.Position(3:4) = handle.Position(3:4) * 3;
        handle.ImageClickedFcn = @mouseClicker;
        handle.Tooltip = sprintf("%s: (left, bottom, width, height)= %d, %d, %d, %d", handle.ImageSource, handle.Position);
    end

    btn = uibutton(fig);
    btn.Position = [10 70 145 22];
    btn.Text = "Random Position";

    spn = uispinner(fig);
    spn.Position = [165 70 125 22];
    spn.Limits = [1 3];

    ta = uitextarea(fig);
    ta.Position = [10 10 280 50];

    btn.ButtonPushedFcn = @(~, ~) randomMize([peppers, street, nebula], ta, spn);

    fig.UserData = struct("peppers", peppers, "street", street, "nebula", nebula, "btn", btn, "spn", spn, "ta", ta);
end

function randomMize(imgs, outputArea, choice)

    idx = choice.Value;
    img = imgs(idx);
    img.Position(1:2) = randi([10 500], 1, 2);
    outputArea.Value = sprintf("New position for %s: %d, %d\n", img.ImageSource, img.Position(1), img.Position(2));
    img.Tooltip = sprintf("%s: (left, bottom, width, height)= %d, %d, %d, %d", img.ImageSource, img.Position);
end

function mouseClicker(src, ~)
    uistack(src, "up");
end
