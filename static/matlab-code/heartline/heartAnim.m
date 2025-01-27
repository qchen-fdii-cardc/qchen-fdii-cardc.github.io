function fig = heartAnim
fig = uifigure("Name", "Circle Rolling Line Animation");
fig.Visible = 'off';
fig.Position(3:4) = [560, 750];
movegui(fig, 'center');


layout = uigridlayout(fig, [5, 3]);
layout.RowHeight = {40, 40, 40, '1x', 40};
layout.ColumnWidth = {'1x', '3x', '1x'};

ax = uiaxes(layout);
ax.Layout.Row = 4;
ax.Layout.Column = [1, 3];

% UI controls
t1_label = uilabel(layout);
t1_label.Text = "$\theta_1^0$";
t1_label.Interpreter = 'latex';
t1_label.Layout.Row = 1;
t1_label.Layout.Column = 1;
% right aligned
t1_label.HorizontalAlignment = 'right';

slider = uislider(layout);
slider.Limits = [0, 2*pi];
slider.Value = 0;
slider.Layout.Row = 1;
slider.Layout.Column = 2;
% make it n * pi
slider.MajorTicks = 0:pi/2:2*pi;
slider.MajorTickLabels = {'0°', '90°', '180°', '270°', '360°'};
slider.MinorTicks = 0:pi/12:2*pi;

t1_value = uieditfield(layout);
t1_value.Layout.Row = 1;
t1_value.Layout.Column = 3;
t1_value.Value = "0°";
t1_value.Editable = false;

slider.ValueChangedFcn = @(~, ~) updateTheta(slider, t1_value);

t2_label = uilabel(layout);
t2_label.Text = "$\theta_2^0$";
t2_label.Interpreter = 'latex';
t2_label.Layout.Row = 2;
t2_label.Layout.Column = 1;
t2_label.HorizontalAlignment = 'right';

slider2 = uislider(layout);
slider2.Limits = [0, 2*pi];
slider2.Value = 0;
slider2.Layout.Row = 2;
slider2.Layout.Column = 2;
% make it n * pi
slider2.MajorTicks = 0:pi/2:2*pi;
slider2.MajorTickLabels = {'0°', '90°', '180°', '270°', '360°'};
slider2.MinorTicks = 0:pi/12:2*pi;

t2_value = uieditfield(layout);
t2_value.Layout.Row = 2;
t2_value.Layout.Column = 3;
t2_value.Value = "0°";
t2_value.Editable = false;

slider2.ValueChangedFcn = @(~, ~) updateTheta(slider2, t2_value);

checkboxOutput = uicheckbox(layout);
checkboxOutput.Text = "==>GIF";
checkboxOutput.Layout.Row = 3;
checkboxOutput.Layout.Column = 1;
checkboxOutput.Value = false;

outputFileName = uieditfield(layout);
outputFileName.Layout.Row = 3;
outputFileName.Layout.Column = 2;
outputFileName.Value = "heart_anim";

startButton = uibutton(layout);
startButton.Text = "Start";
startButton.Layout.Row = 3;
startButton.Layout.Column = 3;

setupGrid = uigridlayout(layout, [1, 7]);
setupGrid.Layout.Row = 5;
setupGrid.Layout.Column = [1, 3];
setupGrid.ColumnWidth = {30, '1x', 30, '1x', 30, 30, '1x'};



r2RatioLabel = uilabel(setupGrid);
r2RatioLabel.Text = "$r_2/r_1$";
r2RatioLabel.Interpreter = 'latex';
r2RatioLabel.Layout.Row = 1;
r2RatioLabel.Layout.Column = 1;
r2RatioLabel.HorizontalAlignment = 'right';

r2RatioSpinner = uispinner(setupGrid);
r2RatioSpinner.Limits = [0.1, 2.0];
r2RatioSpinner.Value = 1.0;
r2RatioSpinner.Step = 0.05;
r2RatioSpinner.Layout.Row = 1;
r2RatioSpinner.Layout.Column = 2;

nPiLabel = uilabel(setupGrid);
nPiLabel.Text = "$\theta$";
nPiLabel.Interpreter = 'latex';
nPiLabel.Layout.Row = 1;
nPiLabel.Layout.Column = 3;
nPiLabel.HorizontalAlignment = 'right';

nPiSpiner = uispinner(setupGrid);
nPiSpiner.Limits = [2, 20];
nPiSpiner.Value = 2.0;
nPiSpiner.Step = 1.0;
nPiSpiner.Layout.Row = 1;
nPiSpiner.Layout.Column = 4;

piLabel = uilabel(setupGrid);
piLabel.Text = "$\pi$";
piLabel.Interpreter = 'latex';
piLabel.Layout.Row = 1;
piLabel.Layout.Column = 5;
piLabel.HorizontalAlignment = 'left';

axesBoundLabel = uilabel(setupGrid);
axesBoundLabel.Text = "$L_{x,y}$";
axesBoundLabel.Interpreter = 'latex';
axesBoundLabel.Layout.Row = 1;
axesBoundLabel.Layout.Column = 6;
axesBoundLabel.HorizontalAlignment = 'right';

axesBoundSpinner = uispinner(setupGrid);
axesBoundSpinner.Limits = [2, 10];
axesBoundSpinner.Value = 3.5;
axesBoundSpinner.Step = 0.5;
axesBoundSpinner.Layout.Row = 1;
axesBoundSpinner.Layout.Column = 7;

axesBoundSpinner.ValueChangedFcn = @(~, ~) setupLim(ax, axesBoundSpinner.Value);

% x = linspace(-2, 2, 1000);
% y = sqrt(1 - x.^2) + sqrt(abs(x));
% line(ax, x, y, 'Color', 'r', 'LineWidth', 2);
xlim(ax, [-axesBoundSpinner.Value, axesBoundSpinner.Value]);
ylim(ax, [-axesBoundSpinner.Value, axesBoundSpinner.Value]);
box(ax, 'on')
axis(ax, 'equal')
axis(ax, 'tight')
title(ax, "Rolling Animation")


startButton.ButtonPushedFcn = @(~, ~) heartAnimPlot(ax, ...
    slider.Value, slider2.Value, ...
    checkboxOutput.Value, outputFileName.Value, ...
    startButton, axesBoundSpinner.Value, ...
    r2RatioSpinner.Value, nPiSpiner.Value ...
    );


fig.Visible = 'on';
end

function setupLim(ax, lim)
xlim(ax, [-lim, lim]);
ylim(ax, [-lim, lim]);
end


function updateTheta(slider, editField)
angle = rad2deg(slider.Value);
editField.Value = sprintf("%.0f°", angle);
end

function heartAnimPlot(ax, theta10, theta20, toGif, gifName, btn, axesBound, r2Ratio, nPi)

btn.Enable = 'off';
cla(ax);
xlim(ax, [-axesBound, axesBound]);
ylim(ax, [-axesBound, axesBound]);
axis(ax, "tight")
axis(ax, 'equal')
box(ax, "on")
axis(ax, "on")
r1 = 1;
r2 = r2Ratio * r1;
% nPi = 8.0;

t1 = @(dt)theta10 + dt;
t2 = @(r1, r2, dt)theta20 + (r1+r2)/r2 * dt;

dts = linspace(0, nPi * pi, 10000);

theta1 = t1(dts);
theta2 = t2(r1, r2, dts);
heartx = (r1+r2) * cos(theta1) + r2 * cos(theta2);
hearty = (r1+r2) * sin(theta1) + r2 * sin(theta2);

% calculate gifname based on theta10 and theta20
gifName = sprintf("%s_%.0f_%.0f.gif", gifName, rad2deg(theta10), rad2deg(theta20));

dtt = (theta1 - theta2) / pi;
while any(dtt >= 2)
    dtt(dtt >= 2) = dtt(dtt >= 2) - 2;
end
while any(dtt <= 0)
    dtt(dtt <= 0) = dtt(dtt <= 0) + 2;
end

[~, idx] = min(abs(dtt-1));

heartx0 = (r1+r2) * cos(theta1(idx)) + r2 * cos(theta2(idx));
hearty0 = (r1+r2) * sin(theta1(idx)) + r2 * sin(theta2(idx));
try
    heartLineLatexString = heartLineLatex(theta10, theta20, heartx0, hearty0, r1, r2);
    ax.Title.String = sprintf("$\\rho(t)\\approx %s$", heartLineLatexString);
    ax.Title.Interpreter = 'latex';
catch
    % disp(ex)
end

dts = linspace(0, nPi * pi, 50);

for dti = 1:length(dts)
    dt = dts(dti);
    cla(ax);
    hold(ax, 'on');
    
    theta1 = t1(dt);
    theta2 = t2(r1, r2, dt);
    [~, ~] = circle(ax, 0, 0, r1, theta1, true);
    c2x = (r1+r2) * cos(theta1);
    c2y = (r1+r2) * sin(theta1);
    [px, py] = circle(ax, c2x, c2y, r2, theta2, true, theta2);
    plot(ax, heartx, hearty, 'r', 'LineWidth', 3)
    plot(ax, [0, c2x, px], [0, c2y, py], 'g', 'Marker', 'o', 'MarkerSize', 5, 'LineWidth', 2)
    
    plot(ax, [heartx0, px], [hearty0, py], 'c', 'LineWidth', 2)
    
    text(ax, c2x, c2y, sprintf('\\theta_{2}=%.0f°',  rad2deg(theta2)), 'FontSize', 14)
    text(ax, 0, 0, sprintf('\\theta_{1}=%.0f°', rad2deg(theta1)), 'FontSize', 14)
    xlim(ax, [-axesBound, axesBound]);
    ylim(ax, [-axesBound, axesBound]);
    drawnow
    
    if toGif
        if dti == 1
            append = false;
        else
            append = true;
        end
        exportgraphics(ax, gifName, 'Append', append);
    end
end
btn.Enable = 'on';

end

function heartLineLatexString = heartLineLatex(theta10, theta20, hx0, hy0, r1, r2)
t = sym('t');
theta1 = t + theta10;
theta2 = (r1+r2)/ r2 * t + theta20;
heartx = (r1+r2) * cos(theta1) + r2 * cos(theta2);
hearty = (r1+r2) * sin(theta1) + r2 * sin(theta2);
rho = expand((heartx - hx0)^2 + (hearty - hy0)^2);
old = digits(2);
rho = simplify(rho);
rho = vpa(mapSymType(rho, 'vpareal', @(x)piecewise(abs(x)<=1e-6, 0, x)), 2);
rho = mapSymType(rho, 'vpareal', @(x)sym(round(x)));
rho = simplify(sqrt(rho),  'IgnoreAnalyticConstraints',1);
heartLineLatexString = latex(rho);
digits(old);
end


function [px, py] = circle(ax, x, y, r, tp, show_circle, start_theta)
if nargin < 5
    tp = [];
end
if nargin < 6
    show_circle = false;
end
if nargin < 7
    start_theta = 0;
end
% CIRCLE draw a circle on a plot
% CIRCLE(ax, x, y, r) draws a circle with center at (x, y) and radius r on
% highlit point with theta tp
n = 20;
k = 50;
t = linspace(0, 2*pi, k * n) + start_theta;
hs = ishold(ax);

hold(ax, 'on');

if show_circle
    % plot(ax, x + r*cos(t), y + r*sin(t), ck);
    % split the circle into 4 parts, and draw each part with different color
    colors = hsv(k);
    for i = 1:k
        idx = (i-1)*n+1:i*n;
        plot(ax, x + r*cos(t(idx)), y + r*sin(t(idx)),...
            'LineWidth', 2, ...
            'LineStyle',':', ...
            'Color', colors(i, :));
        % hline.Color(:) = hline.Color(:) * (i-1) / k;
        % hline.Color = colors(i, :);
    end
end
if isnumeric(tp)
    px = x + r*cos(tp);
    py = y + r*sin(tp);
    plot(ax, px, py, 'r.', 'MarkerSize', 5);
end
if ~hs
    hold(ax, 'off');
end
end


