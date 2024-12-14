t = tcpclient("localhost", port);

myCallback = @(src, ~)fprintf("[Recieved] %s\n", readline(src));

configureCallback(t, "terminator", myCallback);
writeline(t, "Hello, world!");

pause(1);


% clear t