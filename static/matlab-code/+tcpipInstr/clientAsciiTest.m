t = tcpclient('localhost', 8808);

props = properties(t);

for i = 1:length(props)
    fprintf("%30s\t: %40s\n", props{i}, toStr(get(t, props{i})));
end

delete(t);
