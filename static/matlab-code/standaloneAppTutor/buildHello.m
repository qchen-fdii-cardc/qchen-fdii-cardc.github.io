appFile = "hello.m";
buildResults = compiler.build.standaloneApplication(appFile);

% >> buildResults
% buildResults =
%   Results - 属性:

%                   BuildType: 'standaloneApplication'
%                       Files: {2x1 cell}
%     IncludedSupportPackages: {}
%                     Options: [1x1 compiler.build.StandaloneApplicationOptions]

%% package the standalone application

compiler.package.installer(buildResults, ...
    'InstallerName', 'HelloInstall', ...
    'RuntimeDelivery', 'web'...
    );


compiler.package.installer(buildResults, ...
    'InstallerName', 'HelloInstallWithRuntime', ...
    'RuntimeDelivery', 'installer'...
    );