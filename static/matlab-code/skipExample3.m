function skipExample3


f = waitbar(0,'1','Name','Approximating pi...',...
    'CreateCancelBtn','setappdata(gcbf,''canceling'',1)', ...
    'Position', [0, 0, 480, 360]);


movegui(f, 'center');

f.KeyReleaseFcn = @skipABeat;

setappdata(f,'canceling',0);

% Approximate pi^2/8 as: 1 + 1/9 + 1/25 + 1/49 + ...
pisqover8 = 1;
denom = 3;
valueofpi = sqrt(8 * pisqover8);

steps = 20000;
for step = 1:steps
    % Check for clicked Cancel button
    if getappdata(f,'canceling')
        break
    end

    if getappdata(f, 'skip')
        fprintf("Skip step = %d\n", step);
        setappdata(f, 'skip', false);
        continue
    end
    
    % Update waitbar and message
    waitbar(step/steps,f,sprintf('%12.9f',valueofpi))
    
    % Calculate next estimate 
    pisqover8 = pisqover8 + 1 / (denom * denom);
    denom = denom + 2;
    valueofpi = sqrt(8 * pisqover8);
end

delete(f)
end


function skipABeat(src, evt)
    if ismember('control', evt.Modifier) && evt.Key == 'k'
        setappdata(src, 'skip', true);
    end
end
