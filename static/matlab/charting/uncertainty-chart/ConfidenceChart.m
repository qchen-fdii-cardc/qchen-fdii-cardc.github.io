classdef ConfidenceChart < matlab.graphics.chartcontainer.ChartContainer
    %CONFIDENCECHART Summary of this class goes here
    
    properties
        % Public properties
        XData (1,:) double = NaN
        YData (1,:) double = NaN
        YSigma (1,:) double {mustBePositive, mustBeFinite, mustBeScalarOrVector} = 0.15
        MarkerSymbol = 'o'
        Color = [1, 0, 0]
        TitleText = 'Confidence Chart'
        XLabelText = 'X'
        YLabelText = 'Y'
    end
    
    properties (Access = private, Transient, NonCopyable)
        LineObject
        PatchObject
    end
    
    methods (Access = public)
        function obj = title(obj, titleText)
            obj.TitleText = titleText;
        end
        function obj = xlabel(obj, xlabelText)
            obj.XLabelText = xlabelText;
        end
        function obj = ylabel(obj, ylabelText)
            obj.YLabelText = ylabelText;
        end
    end
    
    methods (Access = protected)
        function setup(obj)
            % Create the line plot
            ax = getAxes(obj);
            
            obj.PatchObject = patch(ax, NaN, NaN, 'r', 'FaceAlpha', 0.2, 'EdgeColor', 'none');
            hold(ax, 'on');
            obj.LineObject = plot(ax, obj.XData, obj.YData);
            
            hold(ax, 'off');
        end
        
        function update(obj)
            % Update XData and YData of Line
            obj.LineObject.XData = obj.XData;
            obj.LineObject.YData = obj.YData;
            
            % Update patch XData and YData
            x = obj.XData;
            obj.PatchObject.XData = [x x(end:-1:1)];
            y = obj.YData;
            c = obj.YSigma;
            
            % Validate that ConfidenceMargin is either a scalar or has the same length as XData
            if ~isscalar(c) && numel(c) ~= numel(x)
                error('ConfidenceMargin must be a scalar or have the same length as XData');
            end
            
            obj.PatchObject.YData = [y+c y(end:-1:1)-c];
            
            % Update colors
            obj.LineObject.Color = obj.Color;
            obj.PatchObject.FaceColor = obj.Color;
            
            % Update markers
            obj.LineObject.Marker = obj.MarkerSymbol;
            
            % Update title and labels
            ax = getAxes(obj);
            title(ax, obj.TitleText);
            xlabel(ax, obj.XLabelText);
            ylabel(ax, obj.YLabelText);
        end
    end
end

% Local validation function
function mustBeScalarOrVector(value)
% This function validates that the input is either a scalar or a vector.
% The actual length validation happens in the update method.

% Check if the value is a scalar
if isscalar(value)
    return;
end

% If not a scalar, it should be a vector
if ~isvector(value)
    error('ConfidenceMargin must be a scalar or a vector');
end
end
