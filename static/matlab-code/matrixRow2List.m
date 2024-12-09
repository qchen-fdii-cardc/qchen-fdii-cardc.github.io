function lst = matrixRow2List(m)
% convert a matrix to a list of list
% each row of the matrix is converted to a list
% the list of list is returned
%
% Parameters:
% -----------
% m: matrix
%    the matrix to be converted
%
% Returns:
% --------
% lst: list
%    the list of list
%
% Examples:
% ---------
% m = [1, 2, 3; 4, 5, 6];
% lst = matrixRow2List(m);
% disp(lst);
%
% m = [1, 2, 3];
% lst = matrixRow2List(m);
% disp(lst);

row = size(m, 1);

lstCell = cell(1, row);

for idx = 1:row
    lstCell{idx} = py.list(num2cell(m(idx, :)));
end

lst = py.list(lstCell);