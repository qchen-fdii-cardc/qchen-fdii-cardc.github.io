function mNew = leftUpper(m)
arguments
    m (:, :)
end

mNew = m;
sz = size(m);
n = numel(m);
for i = 1:n
    [row, col] = ind2sub(sz, i);

    % top-right
    if col + row > sz(end) + 1 
    % if col + row > size(1) + 1  % left-bottom
        mNew(row, col) = 0;
    end
end