function board = addRandomTile(board)
    % 找到所有空位置（值为0的位置）
    [rows, cols] = find(board == 0);
    
    % 如果有空位置
    if ~isempty(rows)
        % 随机选择一个空位置
        idx = randi(length(rows));
        row = rows(idx);
        col = cols(idx);
        
        % 90%的概率放置2，10%的概率放置4
        if rand() < 0.9
            board(row, col) = 2;
        else
            board(row, col) = 4;
        end
    end
end 