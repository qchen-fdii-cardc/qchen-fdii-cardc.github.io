labels = {'a', 'b', 'c'};
defaultValues = [1,2,3];
units = {'m/s', 'kg', '$m^2$'};

parameters = editParameters('test', labels, defaultValues, units);
disp(parameters);

parameters = editParameters('test', labels, defaultValues);
disp(parameters);