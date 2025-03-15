classdef HeartCurveApp < matlab.apps.AppBase
    % 心脏线绘制和参数控制应用
    
    properties (Access = public)
        % UI Figure 句柄
        UIFigure matlab.ui.Figure
        
        % 绘图区域
        UIAxes matlab.ui.control.UIAxes
        
        % 控制面板
        ControlPanel matlab.ui.container.Panel
        
        % 参数控制
        SizeSlider matlab.ui.control.Slider
        SizeLabel matlab.ui.control.Label
        RotationSlider matlab.ui.control.Slider
        RotationLabel matlab.ui.control.Label
        ColorDropdown matlab.ui.control.DropDown
        ColorLabel matlab.ui.control.Label
        
        % 显示模式下拉列表（替换原来的按钮组）
        ModeDropdown matlab.ui.control.DropDown
        ModeLabel matlab.ui.control.Label
        
        % 动画控制
        AnimateButton matlab.ui.control.Button
        StopButton matlab.ui.control.Button
        
        % 数据显示
        InfoPanel matlab.ui.container.Panel
        AreaText matlab.ui.control.Label
        LengthText matlab.ui.control.Label
        
        % 方程显示
        EquationPanel matlab.ui.container.Panel
        EquationText matlab.ui.control.Label
    end
    
    properties (Access = private)
        % 心脏线参数
        Size double = 1
        Rotation double = 0
        Color = [0 0.4470 0.7410]
        Mode char = 'polar'
        
        % 动画控制
        AnimationTimer timer
        IsAnimating logical = false
        AnimationProgress double = 0  % 动画进度（0到1）
        
        % 曲线句柄
        CurveLine
        
        ColorMap = struct(...
            'Blue', [0 0.4470 0.7410], ...
            'Red', [0.8500 0.3250 0.0980], ...
            'Yellow', [0.9290 0.6940 0.1250], ...
            'Purple', [0.4940 0.1840 0.5560], ...
            'Green', [0.4660 0.6740 0.1880], ...
            'Cyan', [0.3010 0.7450 0.9330], ...
            'Black', [0 0 0]);
    end
    
    methods (Access = private)
        function updateCurve(app)
            % 更新心脏线显示
            if app.IsAnimating
                switch app.Mode
                    case 'polar'
                        t = linspace(0, app.AnimationProgress * 2*pi, 1000);
                    case 'parametric'
                        t = linspace(0, 2*pi, 1000);
                        numPoints = max(1, round(app.AnimationProgress * 1000));
                        t = t(1:numPoints);
                        if isempty(t)
                            t = 0;
                        end
                end
            else
                t = linspace(0, 2*pi, 1000);
            end
            
            switch app.Mode
                case 'polar'
                    r = app.Size * (1 + cos(t));
                    x = r .* cos(t + app.Rotation);
                    y = r .* sin(t + app.Rotation);
                case 'parametric'
                    x = app.Size * (2*cos(t) - cos(2*t));
                    y = app.Size * (2*sin(t) - sin(2*t));
                    % 应用旋转
                    xr = x*cos(app.Rotation) - y*sin(app.Rotation);
                    yr = x*sin(app.Rotation) + y*cos(app.Rotation);
                    x = xr;
                    y = yr;
            end
            
            % 更新或创建曲线
            if isempty(app.CurveLine) || ~isvalid(app.CurveLine)
                app.CurveLine = plot(app.UIAxes, x, y, 'LineWidth', 2);
            else
                app.CurveLine.XData = x;
                app.CurveLine.YData = y;
            end
            app.CurveLine.Color = app.Color;
            
            % 更新坐标轴范围（使用固定范围以避免动画时的抖动）
            maxRange = app.Size * 4;  % 根据大小参数设置固定范围
            app.UIAxes.XLim = [-maxRange maxRange];
            app.UIAxes.YLim = [-maxRange maxRange];
            
            % 更新信息显示
            area = 1.5 * pi * app.Size^2;
            length = 8 * app.Size;
            app.AreaText.Text = sprintf('面积: %.2f', area);
            app.LengthText.Text = sprintf('周长: %.2f', length);
            
            % 更新方程显示
            switch app.Mode
                case 'polar'
                    app.EquationText.Text = sprintf(['极坐标方程：\n' ...
                        'r = a(1 + cos θ)\n' ...
                        '其中 a = %.2f\n' ...
                        '旋转角度：%.2f rad'], ...
                        app.Size, app.Rotation);
                case 'parametric'
                    app.EquationText.Text = sprintf(['参数方程：\n' ...
                        'x = a(2cos t - cos 2t)\n' ...
                        'y = a(2sin t - sin 2t)\n' ...
                        '其中 a = %.2f\n' ...
                        '旋转角度：%.2f rad'], ...
                        app.Size, app.Rotation);
            end
        end
        
        function startAnimation(app)
            % 开始动画
            if isempty(app.AnimationTimer) || ~isvalid(app.AnimationTimer)
                app.AnimationTimer = timer(...
                    'ExecutionMode', 'fixedRate', ...
                    'Period', 0.02, ...  % 更快的刷新率
                    'TimerFcn', @(~,~) app.animationStep());
            end
            app.AnimationProgress = 0;  % 重置动画进度
            app.IsAnimating = true;
            
            % 更新按钮状态
            app.AnimateButton.Enable = 'off';
            app.StopButton.Enable = 'on';
            
            % 禁用其他控件
            app.ModeDropdown.Enable = 'off';
            app.SizeSlider.Enable = 'off';
            app.RotationSlider.Enable = 'off';
            app.ColorDropdown.Enable = 'off';
            
            start(app.AnimationTimer);
        end
        
        function stopAnimation(app)
            % 停止动画
            if ~isempty(app.AnimationTimer) && isvalid(app.AnimationTimer)
                stop(app.AnimationTimer);
            end
            app.IsAnimating = false;
            app.AnimationProgress = 1;  % 显示完整的心形
            
            % 更新按钮状态
            app.AnimateButton.Enable = 'on';
            app.StopButton.Enable = 'off';
            
            % 启用其他控件
            app.ModeDropdown.Enable = 'on';
            app.SizeSlider.Enable = 'on';
            app.RotationSlider.Enable = 'on';
            app.ColorDropdown.Enable = 'on';
            
            app.updateCurve();
        end
        
        function animationStep(app)
            % 动画步进
            app.AnimationProgress = app.AnimationProgress + 0.005;  % 每步增加0.5%，使动画更平滑
            
            % 如果达到或超过一个完整周期，停止动画
            if app.AnimationProgress >= 1
                app.stopAnimation();
                return;
            end
            
            app.updateCurve();
        end
    end
    
    methods (Access = private)
        function createComponents(app)
            % 创建 UI 组件
            
            % 创建主窗口
            app.UIFigure = uifigure('Name', '心脏线绘制器');
            app.UIFigure.Position = [100 100 800 700];
            
            % 创建主网格布局
            mainGrid = uigridlayout(app.UIFigure, [1 2]);
            mainGrid.ColumnWidth = {210, '1x'};
            mainGrid.Padding = [10 10 10 10];
            mainGrid.RowHeight = {'1x'};
            
            % 创建控制面板
            app.ControlPanel = uipanel(mainGrid);
            app.ControlPanel.Title = '控制面板';
            
            % 控制面板内的网格布局
            controlGrid = uigridlayout(app.ControlPanel, [8 1]);
            controlGrid.RowHeight = {'fit', 'fit', 'fit', 'fit', 'fit', 'fit', 'fit', 'fit'};
            controlGrid.Padding = [5 5 5 5];
            controlGrid.RowSpacing = 15;
            
            % 大小控制区域
            sizeGrid = uigridlayout(controlGrid, [2 1]);
            sizeGrid.RowHeight = {'fit', 'fit'};
            sizeGrid.Layout.Row = 1;
            
            app.SizeLabel = uilabel(sizeGrid);
            app.SizeLabel.Text = '大小';
            app.SizeLabel.Layout.Row = 1;
            
            app.SizeSlider = uislider(sizeGrid);
            app.SizeSlider.Limits = [0.1 5];
            app.SizeSlider.Value = 1;
            app.SizeSlider.Layout.Row = 2;
            app.SizeSlider.ValueChangedFcn = @(~,~) app.sizeChanged();
            
            % 旋转控制区域
            rotationGrid = uigridlayout(controlGrid, [2 1]);
            rotationGrid.RowHeight = {'fit', 'fit'};
            rotationGrid.Layout.Row = 2;
            
            app.RotationLabel = uilabel(rotationGrid);
            app.RotationLabel.Text = '旋转';
            app.RotationLabel.Layout.Row = 1;
            
            app.RotationSlider = uislider(rotationGrid);
            app.RotationSlider.Limits = [0 2*pi];
            app.RotationSlider.Value = 0;
            app.RotationSlider.Layout.Row = 2;
            app.RotationSlider.ValueChangedFcn = @(~,~) app.rotationChanged();
            
            % 颜色选择区域
            colorGrid = uigridlayout(controlGrid, [2 1]);
            colorGrid.RowHeight = {'fit', 'fit'};
            colorGrid.Layout.Row = 3;
            
            app.ColorLabel = uilabel(colorGrid);
            app.ColorLabel.Text = '颜色';
            app.ColorLabel.Layout.Row = 1;
            
            app.ColorDropdown = uidropdown(colorGrid);
            app.ColorDropdown.Items = fieldnames(app.ColorMap);
            app.ColorDropdown.Value = 'Blue';
            app.ColorDropdown.Layout.Row = 2;
            app.ColorDropdown.ValueChangedFcn = @(~,~) app.colorChanged();
            
            % 显示模式下拉列表
            modeGrid = uigridlayout(controlGrid, [2 1]);  % 创建一个两行的网格
            modeGrid.Layout.Row = 4;
            modeGrid.RowHeight = {'fit', 'fit'};
            modeGrid.Padding = [0 0 0 0];
            
            app.ModeLabel = uilabel(modeGrid);
            app.ModeLabel.Text = '显示模式';
            app.ModeLabel.Layout.Row = 1;
            
            app.ModeDropdown = uidropdown(modeGrid);
            app.ModeDropdown.Items = {'极坐标', '参数方程'};
            app.ModeDropdown.Value = '极坐标';
            app.ModeDropdown.Layout.Row = 2;
            app.ModeDropdown.ValueChangedFcn = @(~,~) app.modeChanged();
            
            % 方程显示面板
            app.EquationPanel = uipanel(controlGrid);
            app.EquationPanel.Title = '当前方程';
            app.EquationPanel.Layout.Row = 5;
            
            eqGrid = uigridlayout(app.EquationPanel, [1 1]);
            eqGrid.RowHeight = {'1x'};
            
            app.EquationText = uilabel(eqGrid);
            app.EquationText.WordWrap = 'on';
            app.EquationText.VerticalAlignment = 'top';
            
            % 动画控制按钮
            buttonGrid = uigridlayout(controlGrid, [1 2]);
            buttonGrid.ColumnWidth = {'1x', '1x'};
            buttonGrid.Layout.Row = 6;
            
            app.AnimateButton = uibutton(buttonGrid);
            app.AnimateButton.Text = '开始动画';
            app.AnimateButton.ButtonPushedFcn = @(~,~) app.animateButtonPushed();
            
            app.StopButton = uibutton(buttonGrid);
            app.StopButton.Text = '停止动画';
            app.StopButton.ButtonPushedFcn = @(~,~) app.stopButtonPushed();
            
            % 信息面板
            app.InfoPanel = uipanel(controlGrid);
            app.InfoPanel.Title = '曲线信息';
            app.InfoPanel.Layout.Row = 7;
            
            infoGrid = uigridlayout(app.InfoPanel, [2 1]);
            infoGrid.RowHeight = {'fit', 'fit'};
            
            app.AreaText = uilabel(infoGrid);
            app.AreaText.Text = '面积: 0';
            app.AreaText.Layout.Row = 1;
            
            app.LengthText = uilabel(infoGrid);
            app.LengthText.Text = '周长: 0';
            app.LengthText.Layout.Row = 2;
            
            % 创建绘图区域
            app.UIAxes = uiaxes(mainGrid);
            app.UIAxes.XGrid = 'on';
            app.UIAxes.YGrid = 'on';
            app.UIAxes.Box = 'on';
        end
    end
    
    methods (Access = private)
        % 回调函数
        function sizeChanged(app)
            app.Size = app.SizeSlider.Value;
            app.updateCurve();
        end
        
        function rotationChanged(app)
            app.Rotation = app.RotationSlider.Value;
            app.updateCurve();
        end
        
        function colorChanged(app)
            app.Color = app.ColorMap.(app.ColorDropdown.Value);
            app.updateCurve();
        end
        
        function modeChanged(app)
            if strcmp(app.ModeDropdown.Value, '极坐标')
                app.Mode = 'polar';
            else
                app.Mode = 'parametric';
            end
            app.updateCurve();
        end
        
        function animateButtonPushed(app)
            app.startAnimation();
        end
        
        function stopButtonPushed(app)
            app.stopAnimation();
        end
    end
    
    methods
        function app = HeartCurveApp
            % 构造函数
            
            % 创建 UI 组件
            createComponents(app);
            
            % 初始化显示
            app.updateCurve();
            
            % 初始化按钮状态
            app.StopButton.Enable = 'off';
            
            % 显示 app
            app.UIFigure.Visible = 'on';
        end
        
        function delete(app)
            % 析构函数
            
            % 停止动画
            app.stopAnimation();
            
            % 删除定时器
            if ~isempty(app.AnimationTimer) && isvalid(app.AnimationTimer)
                delete(app.AnimationTimer);
            end
            
            % 删除 UI 组件
            if isvalid(app.UIFigure)
                delete(app.UIFigure);
            end
        end
    end
end