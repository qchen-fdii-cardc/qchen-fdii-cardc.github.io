classdef BucketOptApp < matlab.apps.AppBase
    % 一个显示简单容器优化的App
    % 整个界面分为两栏
    % 界面左边是圆柱形容器的两个参数的滑动调节和容器3D显示
    % 界面右边是设计数值图形，是一个两个y坐标的图
    %   横坐标为地面半径
    %   左边的纵坐标是圆柱体的表面积的大小
    %   右边的纵坐标是高度
    % 约束条件：圆柱体体积为1
    
    % Properties that correspond to app components
    properties (Access = public)
        UIFigure           matlab.ui.Figure
        LeftPanel         matlab.ui.container.Panel
        RightPanel        matlab.ui.container.Panel
        LeftGrid          matlab.ui.container.GridLayout
        ContainerAxes     matlab.ui.control.UIAxes
        ControlPanel      matlab.ui.container.Panel
        RadiusSlider      matlab.ui.control.Slider
        RadiusLabel       matlab.ui.control.Label
        RadiusValue       matlab.ui.control.Label
        HeightValue       matlab.ui.control.Label
        SurfaceAreaValue  matlab.ui.control.Label
        OptimalButton     matlab.ui.control.Button
        PlotAxes          matlab.ui.control.UIAxes
    end
    
    properties (Access = private)
        % 默认参数
        DefaultRadius = 0.5  % 默认半径
        MaxRadius = 2.0      % 最大半径
        MinRadius = 0.1      % 最小半径
        Volume = 1.0         % 固定体积为1
        HasPDEToolbox        % 是否安装了PDE工具箱
        OptimalRadius        % 最优半径
    end
    
    methods (Access = private)
        % 根据半径计算高度（保持体积为1）
        function h = calculateHeight(app, r)
            h = app.Volume / (pi * r^2);
        end
        
        % 计算表面积
        function A = calculateSurfaceArea(app, r, h)
            A = 2 * pi * r * h + 2 * pi * r^2;
        end
        
        % 更新3D容器显示
        function updateContainer(app)
            r = app.RadiusSlider.Value;
            h = calculateHeight(app, r);
            A = calculateSurfaceArea(app, r, h);
            
            % 清除当前axes
            cla(app.ContainerAxes)
            
            if app.HasPDEToolbox
                % 创建圆柱体几何体
                gm = multicylinder(r, h);
                % 绘制几何体
                pdegplot(gm, 'Parent', app.ContainerAxes,  'FaceAlpha', 0.5)
            else
                % 使用基本绘图函数绘制圆柱体
                % 创建圆柱体侧面
                [X,Y,Z] = cylinder(r, 50);
                Z = Z * h;
                
                % 绘制侧面
                surf(app.ContainerAxes, X, Y, Z, 'FaceAlpha', 0.5)
                hold(app.ContainerAxes, 'on')
                
                % 绘制底面
                theta = linspace(0, 2*pi, 50);
                [X,Y] = pol2cart(theta, r);
                Z = zeros(size(X));
                fill3(app.ContainerAxes, X, Y, Z, 'b', 'FaceAlpha', 0.5)
                
                % 绘制顶面
                Z = ones(size(X)) * h;
                fill3(app.ContainerAxes, X, Y, Z, 'b', 'FaceAlpha', 0.5)
                hold(app.ContainerAxes, 'off')
            end
            
            % 设置视角和标题
            view(app.ContainerAxes, 3)
            title(app.ContainerAxes, '容器3D视图')
            xlabel(app.ContainerAxes, 'X')
            ylabel(app.ContainerAxes, 'Y')
            zlabel(app.ContainerAxes, 'Z')
            axis(app.ContainerAxes, 'equal')
            grid(app.ContainerAxes, 'on')
            
            % 更新显示值
            app.RadiusValue.Text = sprintf('半径: %.3f m', r);
            app.HeightValue.Text = sprintf('高度: %.3f m', h);
            app.SurfaceAreaValue.Text = sprintf('表面积: %.3f m²', A);
        end
        
        % 更新数值图表
        function updatePlot(app)
            r = app.RadiusSlider.Value;
            h = calculateHeight(app, r);
            
            % 清除当前axes
            cla(app.PlotAxes)
            
            % 创建半径数组
            r_array = linspace(app.MinRadius, app.MaxRadius, 100);
            
            % 计算对应的表面积和高度
            h_array = app.Volume ./ (pi * r_array.^2);  % 根据体积约束计算高度
            A_array = 2 * pi * r_array .* h_array + 2 * pi * r_array.^2;  % 表面积 = 2πrh + 2πr²
            
            % 创建双y轴图
            yyaxis(app.PlotAxes, 'left')
            p1 = plot(app.PlotAxes, r_array, A_array, 'b-', 'LineWidth', 2);
            ylabel(app.PlotAxes, '表面积 (m²)')
            
            yyaxis(app.PlotAxes, 'right')
            p2 = plot(app.PlotAxes, r_array, h_array, 'r-', 'LineWidth', 2);
            ylabel(app.PlotAxes, '高度 (m)')
            
            xlabel(app.PlotAxes, '半径 (m)')
            title(app.PlotAxes, '容器参数关系图 (体积固定为1m³)')
            grid(app.PlotAxes, 'on')
            
            % 标记当前点
            hold(app.PlotAxes, 'on')
            yyaxis(app.PlotAxes, 'left')
            p3 = plot(app.PlotAxes, r, 2 * pi * r * h + 2 * pi * r^2, 'bo', 'MarkerSize', 10);
            yyaxis(app.PlotAxes, 'right')
            p4 = plot(app.PlotAxes, r, h, 'ro', 'MarkerSize', 10);
            
            % 标记最优点
            r_opt = app.OptimalRadius;
            h_opt = calculateHeight(app, r_opt);
            A_opt = calculateSurfaceArea(app, r_opt, h_opt);
            yyaxis(app.PlotAxes, 'left')
            p5 = plot(app.PlotAxes, r_opt, A_opt, 'g*', 'MarkerSize', 15);
            yyaxis(app.PlotAxes, 'right')
            p6 = plot(app.PlotAxes, r_opt, h_opt, 'g*', 'MarkerSize', 15);
            hold(app.PlotAxes, 'off')
            
            % 添加图例
            legend(app.PlotAxes, [p1, p2, p3, p4, p5, p6], ...
                '表面积曲线', '高度曲线', '当前表面积', '当前高度', '最优表面积', '最优高度', ...
                'Location', 'best')
        end
    end
    
    % Callbacks that handle component events
    methods (Access = private)
        % 半径滑块值改变回调
        function RadiusSliderValueChanged(app, ~)
            updateContainer(app)
            updatePlot(app)
        end
        
        % 最优解按钮回调
        function OptimalButtonPushed(app, ~)
            app.RadiusSlider.Value = app.OptimalRadius;
            updateContainer(app)
            updatePlot(app)
        end
    end
    
    % Component initialization
    methods (Access = private)
        % 创建UI组件
        function createComponents(app)
            % 检查是否安装了PDE工具箱
            app.HasPDEToolbox = license('test', 'PDE_Toolbox');
            
            % 计算最优半径
            app.OptimalRadius = (1/(2*pi))^(1/3);
            
            % 创建UIFigure
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 1200 600];
            app.UIFigure.Name = '容器优化App (体积固定为1m³)';
            
            % 创建左右面板
            app.LeftPanel = uipanel(app.UIFigure);
            app.LeftPanel.Position = [0 0 600 600];
            
            app.RightPanel = uipanel(app.UIFigure);
            app.RightPanel.Position = [600 0 600 600];
            
            % 创建左侧网格布局
            app.LeftGrid = uigridlayout(app.LeftPanel, [2 1]);
            app.LeftGrid.RowHeight = {'3x', 'fit'};
            app.LeftGrid.Padding = [10 10 10 10];
            app.LeftGrid.RowSpacing = 10;
            
            % 创建3D显示区域
            app.ContainerAxes = uiaxes(app.LeftGrid);
            app.ContainerAxes.Layout.Row = 1;
            app.ContainerAxes.Layout.Column = 1;
            
            % 创建控制面板
            app.ControlPanel = uipanel(app.LeftGrid);
            app.ControlPanel.Layout.Row = 2;
            app.ControlPanel.Layout.Column = 1;
            app.ControlPanel.Title = '参数控制';
            
            % 创建控制面板网格布局
            controlGrid = uigridlayout(app.ControlPanel, [6 1]);
            controlGrid.RowHeight = {'1x', 'fit', '1x', '1x', '1x', 'fit'};
            controlGrid.Padding = [10 10 10 10];
            controlGrid.RowSpacing = 5;
            
            % 创建参数显示区域
            app.RadiusLabel = uilabel(controlGrid);
            app.RadiusLabel.Layout.Row = 1;
            app.RadiusLabel.Layout.Column = 1;
            app.RadiusLabel.Text = '半径调节:';
            
            app.RadiusSlider = uislider(controlGrid);
            app.RadiusSlider.Layout.Row = 2;
            app.RadiusSlider.Layout.Column = 1;
            app.RadiusSlider.Limits = [app.MinRadius app.MaxRadius];
            app.RadiusSlider.Value = app.DefaultRadius;
            app.RadiusSlider.ValueChangedFcn = @(s,e) RadiusSliderValueChanged(app, e);
            
            % 创建参数值显示标签
            app.RadiusValue = uilabel(controlGrid);
            app.RadiusValue.Layout.Row = 3;
            app.RadiusValue.Layout.Column = 1;
            app.RadiusValue.Text = sprintf('半径: %.3f m', app.DefaultRadius);
            
            app.HeightValue = uilabel(controlGrid);
            app.HeightValue.Layout.Row = 4;
            app.HeightValue.Layout.Column = 1;
            app.HeightValue.Text = sprintf('高度: %.3f m', calculateHeight(app, app.DefaultRadius));
            
            app.SurfaceAreaValue = uilabel(controlGrid);
            app.SurfaceAreaValue.Layout.Row = 5;
            app.SurfaceAreaValue.Layout.Column = 1;
            app.SurfaceAreaValue.Text = sprintf('表面积: %.3f m²', calculateSurfaceArea(app, app.DefaultRadius, calculateHeight(app, app.DefaultRadius)));
            
            % 创建最优解按钮
            app.OptimalButton = uibutton(controlGrid);
            app.OptimalButton.Layout.Row = 6;
            app.OptimalButton.Layout.Column = 1;
            app.OptimalButton.Text = '显示最优解';
            app.OptimalButton.ButtonPushedFcn = @(s,e) OptimalButtonPushed(app, e);
            
            % 创建图表显示区域
            app.PlotAxes = uiaxes(app.RightPanel);
            app.PlotAxes.Position = [50 50 500 500];
        end
    end
    
    % App creation and startup functions
    methods (Access = public)
        % 创建UIFigure和组件
        function createUIFigure(app)
            createComponents(app)
            
            % 显示UI
            app.UIFigure.Visible = 'on';
        end
    end
    
    % Component initialization
    methods (Access = private)
        % 初始化App
        function startupFcn(app)
            updateContainer(app)
            updatePlot(app)
        end
    end
    
    % App creation and startup functions
    methods (Access = public)
        % 构造函数
        function app = BucketOptApp
            % 创建UIFigure和组件
            createUIFigure(app)
            
            % 运行startup函数
            startupFcn(app)
        end
    end
end