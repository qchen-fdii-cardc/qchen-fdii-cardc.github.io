function gameOver = isGameOver(board)
    [rows, cols] = size(board);
    
    % 检查是否有空格
    if any(board(:) == 0)
        gameOver = false;
        return;
    end
    
    % 检查水平方向是否有可以合并的相邻数字
    for i = 1:rows
        for j = 1:cols-1
            if board(i,j) == board(i,j+1)
                gameOver = false;
                return;
            end
        end
    end
    
    % 检查垂直方向是否有可以合并的相邻数字
    for i = 1:rows-1
        for j = 1:cols
            if board(i,j) == board(i+1,j)
                gameOver = false;
                return;
            end
        end
    end
    
    % 如果没有空格且没有可以合并的相邻数字，则游戏结束
    gameOver = true;
end 