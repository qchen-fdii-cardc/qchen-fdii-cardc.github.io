
doc clibgen.generateLibraryDefinition

clibgen.generateLibraryDefinition("rs2m.h", Libraries="rs2m.dll", OutputFolder="rs2mlib-cpp");

%% find functions
help clib.rs2m

%% test add
help clib.rs2m.add
a = uint64(2);
b = uint64(3);
result = clib.rs2m.add(a, b);
fprintf('2 + 3 = %d\n', result);

%% test linspace
help clib.rs2m.linspace
start = 0;
end_val = 1;
n = 11;
arr = clibArray("clib.rs2m.Double", n);
result = clib.rs2m.linspace(start, end_val, arr);
fprintf('linspace(0, 1, 11) = %s\n', mat2str(result));

%% test square
help clib.rs2m.square
x = 2;
result = clib.rs2m.square(x);
fprintf('square(2) = %d\n', result);

