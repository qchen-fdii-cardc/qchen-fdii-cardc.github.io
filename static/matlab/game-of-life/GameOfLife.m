classdef GameOfLife < handle
    properties
        UIFigure
        MainLayout
        ControlPanel
        GamePanel
        Grid
        Cells
        Board
        Timer
        IsRunning
        CellSize
        GridSize
        Generation
    end
    
    methods
        function app = GameOfLife()
            % 初始化参数
            app.GridSize = 50;
            app.CellSize = 10;
            app.IsRunning = false;
            app.Board = false(app.GridSize);
            app.Generation = 0;
            
            % 创建主窗口
            app.UIFigure = uifigure('Name', 'Game of Life', ...
                'Position', [100, 100, 800, 600], ...
                'CloseRequestFcn', @(src, event) app.closeApp());
            
            % 创建主布局
            app.MainLayout = uigridlayout(app.UIFigure, [1, 2]);
            app.MainLayout.ColumnWidth = {'1x', '4x'};
            app.MainLayout.Padding = [10, 10, 10, 10];
            
            % 创建控制面板
            app.ControlPanel = uipanel(app.MainLayout);
            app.ControlPanel.Title = 'Controls';
            
            % 创建控制按钮
            controlLayout = uigridlayout(app.ControlPanel, [7, 1]);
            controlLayout.RowHeight = {'fit', 'fit', 'fit', 'fit', 'fit', 'fit', 'fit'};
            controlLayout.Padding = [10, 10, 10, 10];
            
            % 播放按钮
            uibutton(controlLayout, ...
                'Text', 'Play', ...
                'ButtonPushedFcn', @(src, event) app.playGame());
            
            % 停止按钮
            uibutton(controlLayout, ...
                'Text', 'Stop', ...
                'ButtonPushedFcn', @(src, event) app.stopGame());
            
            % 单步按钮
            uibutton(controlLayout, ...
                'Text', 'Step', ...
                'ButtonPushedFcn', @(src, event) app.stepGame());
            
            % 随机按钮
            uibutton(controlLayout, ...
                'Text', 'Random', ...
                'ButtonPushedFcn', @(src, event) app.randomizeBoard());
            
            % 清除按钮
            uibutton(controlLayout, ...
                'Text', 'Clear', ...
                'ButtonPushedFcn', @(src, event) app.clearBoard());
                
            % 导出MAT按钮
            uibutton(controlLayout, ...
                'Text', 'Export MAT', ...
                'ButtonPushedFcn', @(src, event) app.exportBoard());
                
            % 导出PNG按钮
            uibutton(controlLayout, ...
                'Text', 'Export PNG', ...
                'ButtonPushedFcn', @(src, event) app.exportPNG());
            
            % 创建游戏面板
            app.GamePanel = uipanel(app.MainLayout);
            app.GamePanel.Title = 'Game Board';
            
            % 创建游戏网格
            app.Grid = uigridlayout(app.GamePanel, [app.GridSize, app.GridSize]);
            app.Grid.Padding = [0, 0, 0, 0];
            app.Grid.RowSpacing = 1;
            app.Grid.ColumnSpacing = 1;
            
            % 初始化细胞
            app.Cells = gobjects(app.GridSize, app.GridSize);
            for i = 1:app.GridSize
                for j = 1:app.GridSize
                    app.Cells(i,j) = uilabel(app.Grid, ...
                        'Text', '', ...
                        'BackgroundColor', [0.8, 0.8, 0.8]);
                end
            end

            movegui(app.UIFigure, 'center');
            
            % 创建定时器
            app.Timer = timer('Period', 0.001, ...
                'ExecutionMode', 'fixedSpacing', ...
                'TimerFcn', @(src, event) app.stepGame(), ...
                'ErrorFcn', @(src, event) app.handleTimerError(event));
        end
        
        function playGame(app)
            if ~app.IsRunning
                app.IsRunning = true;
                start(app.Timer);
            end
        end
        
        function stopGame(app)
            if app.IsRunning
                app.IsRunning = false;
                stop(app.Timer);
            end
        end
        
        function stepGame(app)
            try
                % 创建新的棋盘
                newBoard = false(app.GridSize);
                
                % 更新每个细胞
                for i = 1:app.GridSize
                    for j = 1:app.GridSize
                        % 计算邻居数量（考虑周期性边界）
                        neighbors = app.countNeighbors(i, j);
                        
                        % 应用生命游戏规则
                        if app.Board(i,j)
                            % 活细胞规则
                            newBoard(i,j) = neighbors == 2 || neighbors == 3;
                        else
                            % 死细胞规则
                            newBoard(i,j) = neighbors == 3;
                        end
                    end
                end
                
                % 更新棋盘
                app.Board = newBoard;
                app.Generation = app.Generation + 1;
                app.updateDisplay();
                
                % 更新窗口标题显示代数
                app.UIFigure.Name = sprintf('Game of Life - Generation: %d', app.Generation);
            catch ME
                app.handleError(ME);
            end
        end
        
        function neighbors = countNeighbors(app, i, j)
            % 计算邻居数量（考虑周期性边界）
            neighbors = 0;
            for di = -1:1
                for dj = -1:1
                    if di == 0 && dj == 0
                        continue;
                    end
                    
                    % 计算周期性边界下的邻居位置
                    ni = mod(i + di - 1, app.GridSize) + 1;
                    nj = mod(j + dj - 1, app.GridSize) + 1;
                    
                    if app.Board(ni, nj)
                        neighbors = neighbors + 1;
                    end
                end
            end
        end
        
        function randomizeBoard(app)
            % 随机初始化棋盘
            app.Board = rand(app.GridSize) > 0.7;
            app.Generation = 0;
            app.updateDisplay();
        end
        
        function clearBoard(app)
            % 清除棋盘
            app.Board = false(app.GridSize);
            app.Generation = 0;
            app.updateDisplay();
        end
        
        function updateDisplay(app)
            % 更新显示
            for i = 1:app.GridSize
                for j = 1:app.GridSize
                    if app.Board(i,j)
                        app.Cells(i,j).BackgroundColor = [0, 0.7, 0];
                    else
                        app.Cells(i,j).BackgroundColor = [0.8, 0.8, 0.8];
                    end
                end
            end
            drawnow;
        end
        
        function exportBoard(app)
            try
                % 创建导出数据
                exportData = struct();
                exportData.board = app.Board;
                exportData.generation = app.Generation;
                exportData.gridSize = app.GridSize;
                exportData.timestamp = datestr(now, 'yyyy-mm-dd_HH-MM-SS');
                
                % 获取保存文件名
                [filename, pathname] = uiputfile('*.mat', 'Save Game State', ...
                    sprintf('game_of_life_%s.mat', exportData.timestamp));
                
                if filename ~= 0
                    % 保存数据
                    save(fullfile(pathname, filename), '-struct', 'exportData');
                    uialert(app.UIFigure, 'Game state exported successfully!', 'Export Complete');
                end
            catch ME
                app.handleError(ME);
            end
        end
        
        function exportPNG(app)
            try
                % 获取保存文件名
                [filename, pathname] = uiputfile('*.png', 'Save Game State as PNG', ...
                    sprintf('game_of_life_%s.png', datestr(now, 'yyyy-mm-dd_HH-MM-SS')));
                
                if filename ~= 0
                    % 临时禁用定时器
                    wasRunning = app.IsRunning;
                    if wasRunning
                        app.stopGame();
                    end
                    
                    % 更新面板标题以包含代数信息
                    oldTitle = app.GamePanel.Title;
                    app.GamePanel.Title = sprintf('Game of Life - Generation %d', app.Generation);
                    
                    % 导出游戏面板
                    exportapp(app.UIFigure, fullfile(pathname, filename));
                    
                    % 恢复面板标题
                    app.GamePanel.Title = oldTitle;
                    
                    % 如果之前在运行，重新启动定时器
                    if wasRunning
                        app.playGame();
                    end
                    
                    uialert(app.UIFigure, 'Game state exported as PNG successfully!', 'Export Complete');
                end
            catch ME
                app.handleError(ME);
            end
        end
        
        function handleError(app, ME)
            % 处理错误
            app.stopGame();
            uialert(app.UIFigure, sprintf('Error: %s', ME.message), 'Error');
        end
        
        function handleTimerError(app, event)
            % 处理定时器错误
            app.stopGame();
            uialert(app.UIFigure, sprintf('Timer Error: %s', event.Data.message), 'Timer Error');
        end
        
        function closeApp(app)
            % 关闭应用
            app.stopGame();
            if isvalid(app.Timer)
                delete(app.Timer);
            end
            delete(app.UIFigure);
        end
        
        function delete(app)
            % 清理定时器
            if isvalid(app.Timer)
                stop(app.Timer);
                delete(app.Timer);
            end
        end
    end
end
