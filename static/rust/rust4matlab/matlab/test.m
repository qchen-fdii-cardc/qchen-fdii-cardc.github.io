% Test script for rs2m library
rs2m.unloadLibrary();
rs2m.ensureLibraryLoaded();
% Test add function
disp('Testing add function...');
a = uint64(2);
b = uint64(3);
result = rs2m.add(a, b);
fprintf('2 + 3 = %d\n', result);

% Test linspace function
disp('Testing linspace function...');
result = rs2m.linspace(0, 1, 11);
fprintf('linspace(0, 1, 11) = %s\n', mat2str(result));

% Test square function
disp('Testing square function...');
result = rs2m.square(5);
fprintf('square(5) = %d\n', result);

% Test square function
disp('Testing square function...');
result = rs2m.square(5);
fprintf('5^2 = %f\n', result);

% Unload library when done
rs2m.unloadLibrary();
