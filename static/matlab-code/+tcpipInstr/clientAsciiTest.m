t = tcpclient('localhost', 8808);

props = properties(t);

for i = 1:length(props)
    fprintf("%30s\t: %40s\n", props{i}, toStr(get(t, props{i})));
end

delete(t);

function s = toStr(val)

    if ischar(val)
        s = val;
    elseif isnumeric(val)
        s = num2str(val);
    elseif islogical(val)
        s = string(val);
    elseif class(val) == "function_handle"
        s = strjoin(arrayfun(@func2str, val, 'UniformOutput', false), ', ');
    else
        s = "unknown";
    end

end
