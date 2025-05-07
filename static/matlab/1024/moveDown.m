function board = moveDown(board)
    % 转置矩阵
    board = board';
    [rows, ~] = size(board);
    
    % 对每一行（原矩阵的列）进行处理
    for i = 1:rows
        % 获取当前行
        row = board(i,:);
        % 移除零元素
        row = row(row ~= 0);
        % 合并相同的数字（从右向左合并）
        for j = length(row):-1:2
            if row(j) == row(j-1)
                row(j) = row(j) * 2;
                row(j-1) = 0;
            end
        end
        % 再次移除零元素
        row = row(row ~= 0);
        % 补充零到原始长度（在左侧补充）
        row = [zeros(1, size(board,2)-length(row)), row];
        % 更新board
        board(i,:) = row;
    end
    
    % 转置回原来的方向
    board = board';
end 