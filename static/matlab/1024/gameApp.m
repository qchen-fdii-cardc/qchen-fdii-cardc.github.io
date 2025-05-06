classdef gameApp < handle

    properties
        UIFigure
        Grid
        Board
        Tiles
        SizeSlider
        SizeLabel
        CurrentSize
        MainLayout
    end

    methods

        function app = gameApp()
            % 初始化游戏板大小
            app.CurrentSize = 4;
            
            % 创建UI界面
            app.UIFigure = uifigure('Name', '1024 Game', 'KeyPressFcn', @(src, event) app.handleKeyPress(event));
            
            % 创建主布局
            app.MainLayout = uigridlayout(app.UIFigure, [3, 1]);
            app.MainLayout.RowHeight = {'fit', 'fit', '1x'};
            app.MainLayout.Padding = [10, 10, 10, 10];
            
            % 创建大小选择标签
            app.SizeLabel = uilabel(app.MainLayout, 'Text', 'Board Size: 4');
            
            % 创建大小选择滑块
            app.SizeSlider = uislider(app.MainLayout, ...
                'Limits', [4, 16], ...
                'Value', 4, ...
                'ValueChangedFcn', @(src, event) app.changeBoardSize(event));
            
            % 初始化游戏板
            app.initializeBoard();
        end
        
        function initializeBoard(app)
            % 初始化游戏板
            app.Board = zeros(app.CurrentSize);
            app.Board = addRandomTile(app.Board);
            app.Board = addRandomTile(app.Board);
            
            % 创建网格布局
            if ~isempty(app.Grid)
                delete(app.Grid);
            end
            app.Grid = uigridlayout(app.MainLayout, [app.CurrentSize, app.CurrentSize]);
            app.Grid.RowHeight = repmat({'1x'}, 1, app.CurrentSize);
            app.Grid.ColumnWidth = repmat({'1x'}, 1, app.CurrentSize);
            
            % 初始化方块
            app.Tiles = gobjects(app.CurrentSize, app.CurrentSize);
            for i = 1:app.CurrentSize
                for j = 1:app.CurrentSize
                    app.Tiles(i, j) = uilabel(app.Grid, 'Text', '', ...
                        'HorizontalAlignment', 'center', ...
                        'FontSize', 24, ...
                        'BackgroundColor', [0.8, 0.8, 0.8], ...
                        'FontWeight', 'bold');
                end
            end
            
            % 更新UI
            app.updateUI();
        end
        
        function changeBoardSize(app, event)
            % 获取新的板大小
            newSize = round(event.Value);
            if newSize ~= app.CurrentSize
                app.CurrentSize = newSize;
                app.SizeLabel.Text = sprintf('Board Size: %d', newSize);
                app.initializeBoard();
            end
        end

        function handleKeyPress(app, event)
            % 处理按键事件
            oldBoard = app.Board;  % 保存移动前的状态
            
            switch event.Key
                case 'uparrow'
                    app.Board = moveUp(app.Board);
                case 'downarrow'
                    app.Board = moveDown(app.Board);
                case 'leftarrow'
                    app.Board = moveLeft(app.Board);
                case 'rightarrow'
                    app.Board = moveRight(app.Board);
                otherwise
                    return;
            end
            
            % 只有当板发生变化时才添加新方块
            if ~isequal(oldBoard, app.Board)
                % 添加新的随机方块并更新UI
                app.Board = addRandomTile(app.Board);
                app.updateUI();

                % 检查游戏是否结束
                if isGameOver(app.Board)
                    uialert(app.UIFigure, 'Game Over!', 'Game Over');
                end
            end
        end

        function updateUI(app)
            % 更新UI显示
            for i = 1:app.CurrentSize
                for j = 1:app.CurrentSize
                    value = app.Board(i, j);
                    if value == 0
                        app.Tiles(i, j).Text = '';
                        app.Tiles(i, j).BackgroundColor = [0.8, 0.8, 0.8];
                    else
                        app.Tiles(i, j).Text = num2str(value);
                        app.Tiles(i, j).BackgroundColor = app.getTileColor(value);
                    end
                end
            end
        end

        function color = getTileColor(~, value)
            % 根据方块值返回颜色
            switch value
                case 2, color = [0.9, 0.9, 0.9];
                case 4, color = [0.8, 0.8, 0.6];
                case 8, color = [1.0, 0.6, 0.4];
                case 16, color = [1.0, 0.5, 0.3];
                case 32, color = [1.0, 0.4, 0.2];
                case 64, color = [1.0, 0.3, 0.1];
                case 128, color = [0.9, 0.8, 0.4];
                case 256, color = [0.9, 0.7, 0.3];
                case 512, color = [0.9, 0.6, 0.2];
                case 1024, color = [0.9, 0.5, 0.1];
                otherwise, color = [0.8, 0.8, 0.8];
            end
        end
    end
end
