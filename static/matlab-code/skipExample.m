function skipExample

cleanObj = onCleanup(@cleanAll);

hWaitbar = waitbar(0, 'Iteration 1', 'Name', 'Solving problem', ...
    'CreateCancelBtn', @(~,~)fcn);

hWaitbar.CloseRequestFcn = @(~,~)cleanAll;

setappdata(hWaitbar, 'skip', false);

n = 3;
i = 0;
try
    while(ishandle(hWaitbar))
        if getappdata(hWaitbar, 'skip')
            setappdata(hWaitbar, 'skip', false);
            % for keyboard conformation to continue
            fprintf("%d   - skip this round, ", i)
            fprintf("Press any key to continue...")
            commandwindow; % switch to command window
            pause % delete to just run through
            fprintf("\n");
            continue;
        end
        % pretent to calculate sth
        pause(0.5)
        
        i = i + 1;
        waitbar( mod(i, n) / (n-1),  hWaitbar,  ['Iteration ' num2str(i)]);
    end
catch ME
    fprintf("%s: %s\n", ME.identifier, ME.message);
    cleanAll;
end

end

% set(groot,'ShowHiddenHandles','on')
% delete(get(groot,'Children'))

function cleanAll
delete(gcbf);
end


function fcn
setappdata(gcbf, 'skip', true);
end
