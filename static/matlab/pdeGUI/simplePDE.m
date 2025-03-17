classdef simplePDE < matlab.apps.AppBase
    % 一个简单的PDE工具箱例子的GUI App
    
    properties (Access = public)
        % 用户界面组件
        UIFigure matlab.ui.Figure
    end
    
    properties (Access = private)
        
        % 主布局
        MainLayout matlab.ui.container.GridLayout
        
        % 控制面板
        ControlPanel matlab.ui.container.Panel
        
        % 几何参数控制
        LengthSpinner matlab.ui.control.Spinner      % 长度
        WidthSpinner matlab.ui.control.Spinner       % 宽度
        HeightSpinner matlab.ui.control.Spinner      % 高度
        
        % 材料参数控制
        YoungModulusSpinner matlab.ui.control.Spinner    % 杨氏模量
        PoissonRatioSpinner matlab.ui.control.Spinner    % 泊松比
        
        % 载荷参数控制
        LoadSpinner matlab.ui.control.Spinner        % 载荷大小
        
        % 网格参数
        MeshSizeSpinner matlab.ui.control.Spinner    % 网格尺寸
        
        % 变形显示控制
        DeformationSlider matlab.ui.control.Slider   % 变形比例因子
        DeformationValueLabel matlab.ui.control.Label % 变形比例值显示
        
        % 按钮控件
        GeometryButton matlab.ui.control.Button    % 生成几何体按钮
        MeshButton matlab.ui.control.Button        % 生成网格按钮
        SolveButton matlab.ui.control.Button       % 求解按钮
        
        % 结果显示区域
        ResultsPanel matlab.ui.container.Panel
        TabGroup matlab.ui.container.TabGroup
        GeometryTab matlab.ui.container.Tab
        MeshTab matlab.ui.container.Tab
        ResultsTab matlab.ui.container.Tab
        GeometryAxes matlab.ui.control.UIAxes
        MeshAxes matlab.ui.control.UIAxes
        ResultsAxes matlab.ui.control.UIAxes
        
        % 结果类型选择
        ResultTypeDropDown matlab.ui.control.DropDown
        
        % PDE模型
        Model
        Results
    end
    
    methods (Access = private)
        function createComponents(app)
            % 创建主窗口
            app.UIFigure = uifigure('Name', '悬臂梁有限元分析');
            app.UIFigure.Position = [100 100 1200 800];
            movegui(app.UIFigure,'center');
            
            % 创建主网格布局
            app.MainLayout = uigridlayout(app.UIFigure, [1 2]);
            app.MainLayout.ColumnWidth = {300, '1x'};
            app.MainLayout.Padding = [0 0 0 0];  % 移除边距
            app.MainLayout.RowSpacing = 0;       % 移除行间距
            app.MainLayout.ColumnSpacing = 0;    % 移除列间距
            
            % 创建控制面板
            app.ControlPanel = uipanel(app.MainLayout);
            app.ControlPanel.Title = '参数控制';
            app.ControlPanel.Layout.Column = 1;
            
            % 修改控制面板的网格布局以容纳更多控件
            controlGrid = uigridlayout(app.ControlPanel, [18 2]);
            controlGrid.RowHeight = {25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 35, 25, 25, 25, 25, 25, 25};  % 固定每行高度
            controlGrid.ColumnWidth = {'1x', '1x'};  % 两列等宽
            controlGrid.Padding = [10 10 10 10];
            controlGrid.RowSpacing = 5;  % 减小行间距
            controlGrid.ColumnSpacing = 10;  % 设置列间距
            
            % 几何参数控件
            uilabel(controlGrid, 'Text', '几何参数', 'FontWeight', 'bold');
            uilabel(controlGrid, 'Text', '');
            
            uilabel(controlGrid, 'Text', '长度 (m)');
            app.LengthSpinner = uispinner(controlGrid);
            app.LengthSpinner.Limits = [0.1 10];
            app.LengthSpinner.Value = 1;
            
            uilabel(controlGrid, 'Text', '宽度 (m)');
            app.WidthSpinner = uispinner(controlGrid);
            app.WidthSpinner.Limits = [0.01 1];
            app.WidthSpinner.Value = 0.1;
            
            uilabel(controlGrid, 'Text', '高度 (m)');
            app.HeightSpinner = uispinner(controlGrid);
            app.HeightSpinner.Limits = [0.01 1];
            app.HeightSpinner.Value = 0.1;
            
            % 材料参数控件
            uilabel(controlGrid, 'Text', '材料参数', 'FontWeight', 'bold');
            emptyLabel = uilabel(controlGrid, 'Text', '');
            
            uilabel(controlGrid, 'Text', '杨氏模量 (Pa)');
            app.YoungModulusSpinner = uispinner(controlGrid);
            app.YoungModulusSpinner.Limits = [1e6 1e12];
            app.YoungModulusSpinner.Value = 2e11;  % 钢的杨氏模量
            app.YoungModulusSpinner.Step = 1e9;
            
            uilabel(controlGrid, 'Text', '泊松比');
            app.PoissonRatioSpinner = uispinner(controlGrid);
            app.PoissonRatioSpinner.Limits = [0 0.5];
            app.PoissonRatioSpinner.Value = 0.3;
            app.PoissonRatioSpinner.Step = 0.01;
            
            % 载荷参数控件
            uilabel(controlGrid, 'Text', '载荷参数', 'FontWeight', 'bold');
            uilabel(controlGrid, 'Text', '');
            
            uilabel(controlGrid, 'Text', '载荷 (N)');
            app.LoadSpinner = uispinner(controlGrid);
            app.LoadSpinner.Limits = [-1e6 1e6];
            app.LoadSpinner.Value = -100000;
            app.LoadSpinner.Step = 100;
            
            % 网格参数控件
            uilabel(controlGrid, 'Text', '网格参数', 'FontWeight', 'bold');
            uilabel(controlGrid, 'Text', '');
            
            uilabel(controlGrid, 'Text', '网格尺寸');
            app.MeshSizeSpinner = uispinner(controlGrid);
            app.MeshSizeSpinner.Limits = [0.01 0.5];
            app.MeshSizeSpinner.Value = 0.05;
            app.MeshSizeSpinner.Step = 0.01;
            
            % 变形显示控制
            uilabel(controlGrid, 'Text', '变形显示', 'FontWeight', 'bold');
            uilabel(controlGrid, 'Text', '');
            
            uilabel(controlGrid, 'Text', '变形比例');
            app.DeformationValueLabel = uilabel(controlGrid, 'Text', '500');  % 初始值显示
            
            sliderGrid = uigridlayout(controlGrid, [1 1]);  % 创建滑块的网格布局
            sliderGrid.Layout.Column = [1 2];  % 跨两列
            sliderGrid.Padding = [0 5 0 5];  % 添加适当的上下内边距
            app.DeformationSlider = uislider(sliderGrid);
            app.DeformationSlider.Limits = [10 10000];
            app.DeformationSlider.Value = 500;
            app.DeformationSlider.ValueChangedFcn = @(src,~) deformationValueChanged(app, src);
            app.DeformationSlider.Enable = 'off';  % 初始禁用
            app.DeformationValueLabel.Enable = 'off';  % 初始禁用
            
            % 添加结果类型选择下拉列表
            uilabel(controlGrid, 'Text', '结果类型');
            app.ResultTypeDropDown = uidropdown(controlGrid);
            app.ResultTypeDropDown.Items = {'位移大小', '应力von Mises', 'x方向位移', 'y方向位移', 'z方向位移'};
            app.ResultTypeDropDown.Value = '位移大小';
            app.ResultTypeDropDown.ValueChangedFcn = @(~,~) updateResultsDisplay(app);
            app.ResultTypeDropDown.Enable = 'off';  % 初始禁用
            
            % 添加按钮控件（在控制面板网格布局中）
            % 生成几何体按钮
            app.GeometryButton = uibutton(controlGrid, 'Text', '生成几何体');
            app.GeometryButton.Layout.Column = [1 2];
            app.GeometryButton.ButtonPushedFcn = @(~,~) generateGeometry(app);
            
            % 生成网格按钮
            app.MeshButton = uibutton(controlGrid, 'Text', '生成网格');
            app.MeshButton.Layout.Column = [1 2];
            app.MeshButton.Enable = 'off';  % 初始禁用
            app.MeshButton.ButtonPushedFcn = @(~,~) generateMesh(app);
            
            % 求解按钮
            app.SolveButton = uibutton(controlGrid, 'Text', '求解');
            app.SolveButton.Layout.Column = [1 2];
            app.SolveButton.Enable = 'off';  % 初始禁用
            app.SolveButton.ButtonPushedFcn = @(~,~) solvePDE(app);
            
            % 创建结果显示面板
            app.ResultsPanel = uipanel(app.MainLayout);
            app.ResultsPanel.Title = '计算结果';
            app.ResultsPanel.Layout.Column = 2;
            
            % 创建结果区域的网格布局
            resultsGrid = uigridlayout(app.ResultsPanel, [1 1]);
            resultsGrid.Padding = [0 0 0 0];  % 移除内边距
            
            % 创建TabGroup并使其填满整个网格
            app.TabGroup = uitabgroup(resultsGrid);
            app.TabGroup.Layout.Row = 1;
            app.TabGroup.Layout.Column = 1;
            
            % 创建各个标签页
            app.GeometryTab = uitab(app.TabGroup, 'Title', '几何体');
            app.MeshTab = uitab(app.TabGroup, 'Title', '网格');
            app.ResultsTab = uitab(app.TabGroup, 'Title', '结果');
            
            % 为每个标签页创建网格布局
            geomGrid = uigridlayout(app.GeometryTab, [1 1]);
            meshGrid = uigridlayout(app.MeshTab, [1 1]);
            resultGrid = uigridlayout(app.ResultsTab, [1 1]);
            
            % 设置网格布局的属性
            grids = {geomGrid, meshGrid, resultGrid};
            for g = grids
                g{1}.Padding = [0 0 0 0];
            end
            
            % 创建坐标轴
            app.GeometryAxes = uiaxes(geomGrid);
            app.MeshAxes = uiaxes(meshGrid);
            app.ResultsAxes = uiaxes(resultGrid);
            
            % 设置所有坐标轴的基本属性
            axesList = [app.GeometryAxes, app.MeshAxes, app.ResultsAxes];
            for ax = axesList
                ax.XGrid = 'on';
                ax.YGrid = 'on';
                ax.ZGrid = 'on';
                ax.Box = 'on';
                view(ax, 30, 30);
            end
            
            % 添加标签切换回调
            app.TabGroup.SelectionChangedFcn = @(~,~) tabChanged(app);
        end
        
        function tabChanged(app)
            % 根据当前选择的标签更新显示
            if isempty(app.Model)
                return;
            end
            
            switch app.TabGroup.SelectedTab.Title
                case '几何体'
                    cla(app.GeometryAxes);
                    pdegplot(app.Model.Geometry,'FaceLabels','on','FaceAlpha',0.5,...
                        'Parent',app.GeometryAxes);
                    app.ResultTypeDropDown.Enable = 'off';
                    app.DeformationSlider.Enable = 'off';
                    app.DeformationValueLabel.Enable = 'off';
                    
                case '网格'
                    cla(app.MeshAxes);
                    pdeplot3D(app.Model.Mesh,'Parent',app.MeshAxes);
                    app.ResultTypeDropDown.Enable = 'off';
                    app.DeformationSlider.Enable = 'off';
                    app.DeformationValueLabel.Enable = 'off';
                    
                case '结果'
                    if ~isempty(app.Results)
                        app.ResultTypeDropDown.Enable = 'on';
                        app.DeformationSlider.Enable = 'on';
                        app.DeformationValueLabel.Enable = 'on';
                        updateResultsDisplay(app);
                    end
            end
            
        end
        
        function updateResultsDisplay(app)
            if ~isempty(app.Results)
                cla(app.ResultsAxes);
                
                % 预处理：根据选择确定要显示的数据
                switch app.ResultTypeDropDown.Value
                    case '位移大小'
                        colorData = app.Results.Displacement.Magnitude;
                        titleStr = '位移大小';
                        isDisplacement = true;
                    case '应力von Mises'
                        colorData = app.Results.VonMisesStress;
                        titleStr = 'von Mises应力';
                        isDisplacement = false;
                    case 'x方向位移'
                        colorData = app.Results.Displacement.x;
                        titleStr = 'X方向位移';
                        isDisplacement = true;
                    case 'y方向位移'
                        colorData = app.Results.Displacement.y;
                        titleStr = 'Y方向位移';
                        isDisplacement = true;
                    case 'z方向位移'
                        colorData = app.Results.Displacement.z;
                        titleStr = 'Z方向位移';
                        isDisplacement = true;
                end
                
                % 自动单位转换
                if isDisplacement
                    maxAbs = max(abs(colorData(:)));
                    if maxAbs >= 1
                        unitStr = 'm';
                        factor = 1;
                    elseif maxAbs >= 1e-3
                        unitStr = 'mm';
                        factor = 1e3;
                    elseif maxAbs >= 1e-6
                        unitStr = 'μm';
                        factor = 1e6;
                    else
                        unitStr = 'nm';
                        factor = 1e9;
                    end
                    colorData = colorData * factor;
                else
                    maxAbs = max(abs(colorData(:)));
                    if maxAbs >= 1e9
                        unitStr = 'GPa';
                        factor = 1e-9;
                    elseif maxAbs >= 1e6
                        unitStr = 'MPa';
                        factor = 1e-6;
                    elseif maxAbs >= 1e3
                        unitStr = 'kPa';
                        factor = 1e-3;
                    else
                        unitStr = 'Pa';
                        factor = 1;
                    end
                    colorData = colorData * factor;
                end
                
                % 统一的绘图调用
                pdeplot3D(app.Results.Mesh,...
                    'ColorMapData', colorData,...
                    'Deformation', app.Results.Displacement,...
                    'DeformationScaleFactor', app.DeformationSlider.Value,...
                    'Parent', app.ResultsAxes);
                
                
                % 添加颜色条和标题
                cb = colorbar(app.ResultsAxes);
                title(app.ResultsAxes, titleStr);
                
                % 设置颜色条标签
                if isDisplacement
                    cb.Label.String = sprintf('位移 (%s)', unitStr);
                else
                    cb.Label.String = sprintf('应力 (%s)', unitStr);
                end
            end
        end
        
        function generateGeometry(app)
            % 创建有限元模型
            app.Model = femodel(AnalysisType="structuralStatic");
            
            % 创建几何
            L = app.LengthSpinner.Value;
            W = app.WidthSpinner.Value;
            H = app.HeightSpinner.Value;
            
            % 定义长方体几何
            gm = multicuboid(L,W,H);
            
            % 将几何导入到模型中
            app.Model.Geometry = gm;
            
            % 切换到几何体标签页并显示
            app.TabGroup.SelectedTab = app.GeometryTab;
            tabChanged(app);
            
            % 启用网格生成按钮
            app.MeshButton.Enable = 'on';
            
        end
        
        function generateMesh(app)
            % 生成网格
            app.Model = generateMesh(app.Model,'Hmax',app.MeshSizeSpinner.Value);
            
            % 切换到网格标签页并显示
            app.TabGroup.SelectedTab = app.MeshTab;
            tabChanged(app);
            
            % 启用求解按钮
            app.SolveButton.Enable = 'on';
            
            
        end
        
        function solvePDE(app)
            app.SolveButton.Enable = 'off';
            generateGeometry(app);
            generateMesh(app);
            % 设置材料属性
            app.Model.MaterialProperties = materialProperties(...
                YoungsModulus=app.YoungModulusSpinner.Value,...
                PoissonsRatio=app.PoissonRatioSpinner.Value);
            
            % 设置边界条件
            % 固定端（F3面，x=0）
            app.Model.FaceBC(3) = faceBC(Constraint="fixed");
            
            % 自由端载荷（F5面，x=L）
            load = app.LoadSpinner.Value;
            app.Model.FaceLoad(5) = faceLoad(SurfaceTraction=[0;0;load]);
            
            % 求解
            app.Results = solve(app.Model);
            
            % 求解完成后自动切换到结果标签
            app.TabGroup.SelectedTab = app.ResultsTab;
            tabChanged(app);
            app.SolveButton.Enable = 'on';
        end
        
        function deformationValueChanged(app, src)
            % 更新标签显示
            app.DeformationValueLabel.Text = sprintf('%.0f', src.Value);
            % 更新显示
            updateResultsDisplay(app);
        end
    end
    
    methods
        function app = simplePDE
            % 构造函数
            createComponents(app);
            
            registerApp(app, app.UIFigure);
            % 显示界面
            app.UIFigure.Visible = 'on';
        end
        
        function delete(app)
            % 析构函数
            if isvalid(app.UIFigure)
                delete(app.UIFigure);
            end
        end
    end
end

