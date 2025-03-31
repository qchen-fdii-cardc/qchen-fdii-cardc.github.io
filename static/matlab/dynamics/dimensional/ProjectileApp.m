classdef ProjectileApp < matlab.apps.AppBase
    properties
        % UI Components
        Figure
        MainGrid      % 主网格布局
        LeftGrid      % 左侧网格布局
        RightGrid     % 右侧网格布局
        DimAxes       % 无量纲坐标轴
        DimensionalAxes % 有量纲坐标轴
        
        % Tab Group
        TabGroup
        TrajectoryTab
        DimensionalTab
        SurfaceTab
        
        % Input Parameters
        MassEdit
        DragCoeffEdit
        GravityEdit
        VelocityEdit
        AngleEdit
        
        % Simulation Parameters
        Lambda          % 无量纲量 mg/(kV^2)
        LengthScale    % 无量纲长度 m/k
        TimeScale      % 无量纲时间 m/(kV)
        Theta
        
        % Display Labels
        LengthScaleLabel
        TimeScaleLabel
        
        % Plot Data
        TimeData
        XData
        YData
        
        % Value Changed Listeners
        MassListener
        DragCoeffListener
        GravityListener
        VelocityListener
        AngleListener
        
        % 最高点显示
        MaxHeightLabel
        MaxHeightNondimLabel
        
        % 3D Plot
        Surface3DAxes    % 三维图坐标轴
        LambdaRange      % lambda 的范围数组
        ThetaRange      % theta 的范围数组
        RangeData      % 存储射程数据的矩阵
        
        % 轨迹数据存储
        TrajectoryData    % 存储所有轨迹数据的结构体数组
        TrajectoryLines   % 存储所有轨迹线的句柄数组
        LegendEntries     % 存储图例条目
        
        % 删除按钮
        ClearButton
    end
    
    methods
        function app = ProjectileApp
            % 初始化基类
            app = app@matlab.apps.AppBase();
            
            % 创建主窗口
            app.Figure = uifigure('Name', 'Projectile Motion Analysis', ...
                'Position', [100 100 1200 600]);
            movegui(app.Figure, 'center');
            
            % 创建主网格布局 - 2列，左窄右宽
            app.MainGrid = uigridlayout(app.Figure, [1 2]);
            app.MainGrid.ColumnWidth = {'3x', '7x'};
            
            % 创建左侧控制面板网格 - 增加行数以容纳无量纲量显示
            app.LeftGrid = uigridlayout(app.MainGrid, [11 2]);
            app.LeftGrid.RowHeight = {'fit', 'fit', 'fit','fit', 'fit', 'fit', 'fit', 'fit', 'fit', 'fit', 'fit'};
            app.LeftGrid.Padding = [10 10 10 10];
            app.LeftGrid.RowSpacing = 10;
            
            % 创建右侧图表网格
            app.RightGrid = uigridlayout(app.MainGrid, [1 1]);
            app.RightGrid.Padding = [10 10 10 10];
            
            % 创建 Tab Group
            app.TabGroup = uitabgroup(app.RightGrid);
            
            % 创建轨迹 Tab
            app.TrajectoryTab = uitab(app.TabGroup);
            app.TrajectoryTab.Title = 'Non-dimensional';
            
            % 创建轨迹 Tab 的网格布局
            trajectoryGrid = uigridlayout(app.TrajectoryTab, [1 1]);
            trajectoryGrid.Padding = [10 10 10 10];
            
            % 创建有量纲轨迹 Tab
            app.DimensionalTab = uitab(app.TabGroup);
            app.DimensionalTab.Title = 'Dimensional';
            
            % 创建有量纲轨迹 Tab 的网格布局
            dimensionalGrid = uigridlayout(app.DimensionalTab, [1 1]);
            dimensionalGrid.Padding = [10 10 10 10];
            
            % 创建三维图 Tab
            app.SurfaceTab = uitab(app.TabGroup);
            app.SurfaceTab.Title = 'Range Surface';
            
            % 创建三维图 Tab 的网格布局
            surfaceGrid = uigridlayout(app.SurfaceTab, [1 1]);
            surfaceGrid.Padding = [10 10 10 10];
            
            % 创建输入控件
            createInputFields(app);
            
            % 创建轨迹图
            app.DimAxes = uiaxes(trajectoryGrid);
            app.DimAxes.Layout.Row = 1;
            app.DimAxes.Layout.Column = 1;
            title(app.DimAxes, 'Non-dimensional Trajectories');
            xlabel(app.DimAxes, 'X (Non-dimensional)');
            ylabel(app.DimAxes, 'Y (Non-dimensional)');
            grid(app.DimAxes, 'on');
            
            % 创建有量纲轨迹图
            app.DimensionalAxes = uiaxes(dimensionalGrid);
            app.DimensionalAxes.Layout.Row = 1;
            app.DimensionalAxes.Layout.Column = 1;
            title(app.DimensionalAxes, 'Dimensional Trajectories');
            xlabel(app.DimensionalAxes, 'x (m)');
            ylabel(app.DimensionalAxes, 'y (m)');
            grid(app.DimensionalAxes, 'on');
            
            % 创建三维图
            app.Surface3DAxes = uiaxes(surfaceGrid);
            app.Surface3DAxes.Layout.Row = 1;
            app.Surface3DAxes.Layout.Column = 1;
            title(app.Surface3DAxes, 'Range Surface');
            xlabel(app.Surface3DAxes, 'λ');
            ylabel(app.Surface3DAxes, 'θ (deg)');
            zlabel(app.Surface3DAxes, 'Range (m)');
            grid(app.Surface3DAxes, 'on');
            view(app.Surface3DAxes, 45, 30);
            
            % 初始化三维图
            calculateRangeSurface(app);
            
            % 初始化无量纲参数
            updateDimensionlessParameters(app);
        end
        
        function createInputFields(app)
            % 质量输入
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Mass (kg):';
            lbl.Layout.Row = 1;
            lbl.Layout.Column = 1;
            
            app.MassEdit = uislider(app.LeftGrid);
            app.MassEdit.Limits = [0.1 10];
            app.MassEdit.Value = 1;
            app.MassEdit.Layout.Row = 1;
            app.MassEdit.Layout.Column = 2;
            
            % 阻力系数输入
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Drag Coefficient (N·s²/m²):';
            lbl.Layout.Row = 2;
            lbl.Layout.Column = 1;
            
            app.DragCoeffEdit = uislider(app.LeftGrid);
            app.DragCoeffEdit.Limits = [0.01 1];
            app.DragCoeffEdit.Value = 0.1;
            app.DragCoeffEdit.Layout.Row = 2;
            app.DragCoeffEdit.Layout.Column = 2;
            
            % 重力加速度输入
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Gravity (m/s²):';
            lbl.Layout.Row = 3;
            lbl.Layout.Column = 1;
            
            app.GravityEdit = uislider(app.LeftGrid);
            app.GravityEdit.Limits = [1 20];
            app.GravityEdit.Value = 9.81;
            app.GravityEdit.Layout.Row = 3;
            app.GravityEdit.Layout.Column = 2;
            
            % 初速度输入
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Initial Velocity (m/s):';
            lbl.Layout.Row = 4;
            lbl.Layout.Column = 1;
            
            app.VelocityEdit = uislider(app.LeftGrid);
            app.VelocityEdit.Limits = [1 50];
            app.VelocityEdit.Value = 10;
            app.VelocityEdit.Layout.Row = 4;
            app.VelocityEdit.Layout.Column = 2;
            
            % 角度输入
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Angle (degrees):';
            lbl.Layout.Row = 5;
            lbl.Layout.Column = 1;
            
            app.AngleEdit = uislider(app.LeftGrid);
            app.AngleEdit.Limits = [0 90];
            app.AngleEdit.Value = 45;
            app.AngleEdit.Layout.Row = 5;
            app.AngleEdit.Layout.Column = 2;
            
            % 无量纲量显示
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Length Scale (m/k):';
            lbl.Layout.Row = 6;
            lbl.Layout.Column = 1;
            
            app.LengthScaleLabel = uilabel(app.LeftGrid);
            app.LengthScaleLabel.Text = '0';
            app.LengthScaleLabel.Layout.Row = 6;
            app.LengthScaleLabel.Layout.Column = 2;
            
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Time Scale (m/kV):';
            lbl.Layout.Row = 7;
            lbl.Layout.Column = 1;
            
            app.TimeScaleLabel = uilabel(app.LeftGrid);
            app.TimeScaleLabel.Text = '0';
            app.TimeScaleLabel.Layout.Row = 7;
            app.TimeScaleLabel.Layout.Column = 2;
            
            % 添加最高点显示
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Max Height (m):';
            lbl.Layout.Row = 8;
            lbl.Layout.Column = 1;
            
            app.MaxHeightLabel = uilabel(app.LeftGrid);
            app.MaxHeightLabel.Text = '0';
            app.MaxHeightLabel.Layout.Row = 8;
            app.MaxHeightLabel.Layout.Column = 2;
            
            lbl = uilabel(app.LeftGrid);
            lbl.Text = 'Max Height (Non-dim):';
            lbl.Layout.Row = 9;
            lbl.Layout.Column = 1;
            
            app.MaxHeightNondimLabel = uilabel(app.LeftGrid);
            app.MaxHeightNondimLabel.Text = '0';
            app.MaxHeightNondimLabel.Layout.Row = 9;
            app.MaxHeightNondimLabel.Layout.Column = 2;
            
            % 创建按钮网格布局
            buttonGrid = uigridlayout(app.LeftGrid, [2 1]);
            buttonGrid.Layout.Row = 10;
            buttonGrid.Layout.Column = [1 2];
            buttonGrid.RowHeight = {'1x', '1x'};
            buttonGrid.RowSpacing = 5;
            
            % 模拟按钮
            btn = uibutton(buttonGrid);
            btn.Text = 'Simulate';
            btn.ButtonPushedFcn = @app.simulateButtonPushed;
            btn.Layout.Row = 1;
            btn.Layout.Column = 1;
            
            % 清空按钮
            app.ClearButton = uibutton(buttonGrid);
            app.ClearButton.Text = 'Clear All';
            app.ClearButton.ButtonPushedFcn = @app.clearButtonPushed;
            app.ClearButton.Layout.Row = 2;
            app.ClearButton.Layout.Column = 1;
            
            % 添加值改变事件监听
            app.MassListener = listener(app.MassEdit, 'ValueChanged', @(~,~) updateDimensionlessParameters(app));
            app.DragCoeffListener = listener(app.DragCoeffEdit, 'ValueChanged', @(~,~) updateDimensionlessParameters(app));
            app.GravityListener = listener(app.GravityEdit, 'ValueChanged', @(~,~) updateDimensionlessParameters(app));
            app.VelocityListener = listener(app.VelocityEdit, 'ValueChanged', @(~,~) updateDimensionlessParameters(app));
            app.AngleListener = listener(app.AngleEdit, 'ValueChanged', @(~,~) updateDimensionlessParameters(app));
        end
        
        function updateDimensionlessParameters(app)
            % 获取当前参数
            m = app.MassEdit.Value;
            k = app.DragCoeffEdit.Value;
            g = app.GravityEdit.Value;
            V = app.VelocityEdit.Value;
            theta_deg = app.AngleEdit.Value;
            
            % 计算无量纲参数
            app.LengthScale = m/k;
            app.TimeScale = m/(k*V);
            app.Lambda = m*g/(k*V^2);
            app.Theta = theta_deg * pi/180;
            
            % 更新显示
            app.LengthScaleLabel.Text = sprintf('%.2f m', app.LengthScale);
            app.TimeScaleLabel.Text = sprintf('%.2f s', app.TimeScale);
            
            % 检查是否已存在相同无量纲参数的轨迹
            if ~isempty(app.TrajectoryData)
                existingParams = [app.TrajectoryData.params];
                for i = 1:length(existingParams)
                    % 计算已存在轨迹的无量纲参数
                    existingLambda = existingParams(i).m * existingParams(i).g / ...
                        (existingParams(i).k * existingParams(i).V^2);
                    existingTheta = existingParams(i).theta * pi/180;
                    
                    % 使用无量纲参数进行比较
                    if abs(existingLambda - app.Lambda) < 1e-6 && ...
                            abs(existingTheta - app.Theta) < 1e-6
                        % 如果找到相同无量纲参数，直接返回
                        return;
                    end
                end
            end
            
            % 求解ODE
            [app.TimeData, app.XData, app.YData] = solveDimensionlessODE(app);
            
            % 计算最高点
            [maxHeight, maxIdx] = max(app.YData);
            maxHeightDim = maxHeight * app.LengthScale;
            
            % 更新最高点显示
            app.MaxHeightLabel.Text = sprintf('%.2f m', maxHeightDim);
            app.MaxHeightNondimLabel.Text = sprintf('%.2f', maxHeight);
            
            % 创建新的轨迹线 - 使用无量纲坐标
            hold(app.DimAxes, 'on');
            newLine = plot(app.DimAxes, app.XData, app.YData, 'LineWidth', 2);
            hold(app.DimAxes, 'off');
            
            % 创建新的轨迹线 - 使用有量纲坐标
            hold(app.DimensionalAxes, 'on');
            newDimensionalLine = plot(app.DimensionalAxes, app.XData * app.LengthScale, app.YData * app.LengthScale, 'LineWidth', 2);
            hold(app.DimensionalAxes, 'off');
            
            % 存储轨迹数据 - 同时存储有量纲和无量纲坐标
            newData = struct('x', app.XData * app.LengthScale, 'y', app.YData * app.LengthScale, ...
                'X', app.XData, 'Y', app.YData, ...
                'params', struct('m', m, 'k', k, 'g', g, 'V', V, 'theta', theta_deg));
            app.TrajectoryData = [app.TrajectoryData, newData];
            app.TrajectoryLines = [app.TrajectoryLines, newLine, newDimensionalLine];
            
            % 更新图例
            legendText = sprintf('λ=%.2f, θ=%.1f°', app.Lambda, theta_deg);
            app.LegendEntries = [app.LegendEntries, {legendText}];
            
            % 在最高点添加文本标注
            [maxY, maxIdx] = max(app.YData);
            text(app.DimAxes, app.XData(maxIdx), maxY, legendText, ...
                'VerticalAlignment', 'bottom', ...
                'HorizontalAlignment', 'center');
            
            % 在有量纲图中添加最高点标注
            text(app.DimensionalAxes, app.XData(maxIdx) * app.LengthScale, maxY * app.LengthScale, legendText, ...
                'VerticalAlignment', 'bottom', ...
                'HorizontalAlignment', 'center');
            
            % 将图例移到坐标轴外部
            legend(app.DimAxes, app.LegendEntries, 'Location', 'eastoutside');
            legend(app.DimensionalAxes, app.LegendEntries, 'Location', 'eastoutside');
            
            % 设置坐标轴范围为自动
            axis(app.DimAxes, 'auto');
            axis(app.DimensionalAxes, 'auto');
            
            % 更新标题
            title(app.DimAxes, sprintf('Non-dimensional Trajectories (λ=%.2f, θ=%.1f°)', app.Lambda, theta_deg));
            title(app.DimensionalAxes, sprintf('Dimensional Trajectories (λ=%.2f, θ=%.1f°)', app.Lambda, theta_deg));
            
            % 更新三维图
            calculateRangeSurface(app);
        end
        
        function simulateButtonPushed(app, ~, ~)
            % 直接调用updateDimensionlessParameters来更新轨迹
            updateDimensionlessParameters(app);
        end
        
        function [T, X, Y] = solveDimensionlessODE(app)
            % 初始条件
            X0 = 0;
            Y0 = 0;
            Xp0 = cos(app.Theta);
            Yp0 = sin(app.Theta);
            
            % 求解ODE直到Y回到初始高度
            y0 = [X0; Y0; Xp0; Yp0];
            options = odeset('Events', @(t,y) heightEvent(t,y,Y0), ...
                'RelTol', 1e-8, ...  % 相对误差
                'AbsTol', 1e-8, ...  % 绝对误差
                'MaxStep', 0.01);    % 最大步长
            [T, Y] = ode45(@(t,y) dimensionlessODE(t, y, app.Lambda), [0 100], y0, options);
            
            X = Y(:,1);
            Y = Y(:,2);
        end
        
        function calculateRangeSurface(app)
            % 创建lambda和theta的网格
            app.LambdaRange = linspace(0.01, 100, 20);
            app.ThetaRange = linspace(1, 90, 20);  % 从1度开始，而不是0度
            [Lambda, Theta] = meshgrid(app.LambdaRange, app.ThetaRange);
            app.RangeData = zeros(size(Lambda));
            
            % 计算每个点的最终x距离
            for i = 1:length(app.ThetaRange)
                for j = 1:length(app.LambdaRange)
                    lambda = Lambda(i,j);
                    theta = Theta(i,j) * pi/180;
                    
                    % 求解ODE直到Y回到初始高度
                    y0 = [0; 0; cos(theta); sin(theta)];
                    options = odeset('Events', @(t,y) heightEvent(t,y,0), ...
                        'RelTol', 1e-8, ...
                        'AbsTol', 1e-8, ...
                        'MaxStep', 0.01);
                    [~, Y] = ode45(@(t,y) dimensionlessODE(t, y, lambda), [0 100], y0, options);
                    
                    % 检查是否有解
                    if ~isempty(Y)
                        
                        % 获取最终x距离（转换为有量纲）
                        % Y(end,1)是物体落地时的x坐标，Y(end,2)是y坐标（应该接近0）
                        if abs(Y(end,2)) < 1e-6  % 确保物体确实落地
                            app.RangeData(i,j) = Y(end,1) * lambda;
                        else
                            % 如果物体没有正确落地，设置为NaN
                            app.RangeData(i,j) = NaN;
                        end
                    else
                        % 如果没有解，设置为NaN
                        app.RangeData(i,j) = NaN;
                    end
                end
            end
            
            % 绘制三维图
            surf(app.Surface3DAxes, Lambda, Theta, app.RangeData);
            colorbar(app.Surface3DAxes);
            shading(app.Surface3DAxes, 'interp');
            
            % 更新标签
            title(app.Surface3DAxes, 'Range Surface');
            xlabel(app.Surface3DAxes, 'λ');
            ylabel(app.Surface3DAxes, 'θ (deg)');
            zlabel(app.Surface3DAxes, 'Range (m)');
            
            % 在当前参数位置添加标记
            % hold(app.Surface3DAxes, 'on');
            % plot3(app.Surface3DAxes, app.Lambda, app.Theta*180/pi, ...
            %     app.XData(end) * app.LengthScale, 'ro', 'MarkerSize', 10, 'MarkerFaceColor', 'r');
            % hold(app.Surface3DAxes, 'off');
        end
        
        function clearButtonPushed(app, ~, ~)
            % 清除所有轨迹线
            if ~isempty(app.TrajectoryLines)
                delete(app.TrajectoryLines);
                app.TrajectoryLines = [];
            end
            
            % 清除所有轨迹数据
            app.TrajectoryData = [];
            
            % 清除图例
            app.LegendEntries = {};
            legend(app.DimAxes, 'off');
            legend(app.DimensionalAxes, 'off');
            
            % 清除坐标轴上的文本标注
            for ax = [app.DimAxes, app.DimensionalAxes]
                children = get(ax, 'Children');
                for i = 1:length(children)
                    if isa(children(i), 'matlab.graphics.primitive.Text')
                        delete(children(i));
                    end
                end
            end
            
            % 重置坐标轴
            axis(app.DimAxes, 'auto');
            axis(app.DimensionalAxes, 'auto');
            
            % 更新标题
            title(app.DimAxes, 'Non-dimensional Trajectories');
            xlabel(app.DimAxes, 'X (Non-dimensional)');
            ylabel(app.DimAxes, 'Y (Non-dimensional)');
            
            title(app.DimensionalAxes, 'Dimensional Trajectories');
            xlabel(app.DimensionalAxes, 'x (m)');
            ylabel(app.DimensionalAxes, 'y (m)');
            
            % 更新三维图
            calculateRangeSurface(app);
        end
    end
end

function [value, isterminal, direction] = heightEvent(t, y, Y0)
% 当Y回到初始高度时停止
value = y(2) - Y0;      % 当前高度与初始高度的差
isterminal = 1;         % 停止积分
direction = -1;         % 只在下降穿过初始高度时停止
end

function dydt = dimensionlessODE(t, y, lambda)
% y = [X; Y; X'; Y']
X = y(1);
Y = y(2);
Xp = y(3);
Yp = y(4);

% Calculate velocity magnitude
V = sqrt(Xp^2 + Yp^2);

% ODE system
dydt = zeros(4,1);
dydt(1) = Xp;
dydt(2) = Yp;
dydt(3) = -V * Xp;
dydt(4) = -V * Yp - lambda;
end