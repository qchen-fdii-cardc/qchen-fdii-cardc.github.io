classdef SimpleFaceAverageApp < matlab.apps.AppBase
    % SimpleFaceAverageApp
    % Modern MATLAB app using UIFigure and the App Framework.
    % This is a new implementation and does not modify the original GUIDE version.

    properties (Access = private)
        UIFigure matlab.ui.Figure = matlab.ui.Figure.empty
        MainGrid matlab.ui.container.GridLayout = matlab.ui.container.GridLayout.empty
        HeaderPanel matlab.ui.container.Panel = matlab.ui.container.Panel.empty
        HeaderLabel matlab.ui.control.Label = matlab.ui.control.Label.empty
        StatusLabel matlab.ui.control.Label = matlab.ui.control.Label.empty
        LoadButton matlab.ui.control.Button = matlab.ui.control.Button.empty
        DetectButton matlab.ui.control.Button = matlab.ui.control.Button.empty
        AverageButton matlab.ui.control.Button = matlab.ui.control.Button.empty
        CloseButton matlab.ui.control.Button = matlab.ui.control.Button.empty
        ContentGrid matlab.ui.container.GridLayout = matlab.ui.container.GridLayout.empty
        InputPanel matlab.ui.container.Panel = matlab.ui.container.Panel.empty
        InputAxes matlab.ui.control.UIAxes = matlab.ui.control.UIAxes.empty
        ResultPanel matlab.ui.container.Panel = matlab.ui.container.Panel.empty
        ResultAxes matlab.ui.control.UIAxes = matlab.ui.control.UIAxes.empty
        InstructionLabel matlab.ui.control.Label = matlab.ui.control.Label.empty
        ImageData {mustBeNumeric} = []
        FaceBoxes double = zeros(0, 4)
        FaceIncluded logical = false(0, 1)
        FaceTable matlab.ui.control.Table = matlab.ui.control.Table.empty
        AveragedImage uint8 = uint8([])
        LastResultClickTime double = NaN
        initialFolder char = '';
    end

    methods (Access = public)
        % Public entry point: create and show the app window.
        function app = SimpleFaceAverageApp()
            app.createUI();
            app.initialFolder = app.getAppFolder();
        end
    end

    methods (Access = private)

        function createUI(app)
            app.UIFigure = uifigure('Name', 'Simple Face Average', ...
                'Position', [100 100 1180 760], ...
                'Color', [0.97 0.97 0.97], ...
                'Resize', 'on');
            app.UIFigure.CloseRequestFcn = @(src, event)app.closeApp();

            app.MainGrid = uigridlayout(app.UIFigure, [2 1]);
            app.MainGrid.RowHeight = {132, '1x'};
            app.MainGrid.ColumnWidth = {'1x'};
            app.MainGrid.Padding = [20 20 20 20];
            app.MainGrid.RowSpacing = 14;
            app.MainGrid.ColumnSpacing = 14;

            app.HeaderPanel = uipanel(app.MainGrid, 'Title', '');
            app.HeaderPanel.Layout.Row = 1;
            app.HeaderPanel.Layout.Column = 1;
            app.HeaderPanel.BackgroundColor = [1 1 1];

            app.HeaderLabel = uilabel(app.HeaderPanel, ...
                'Text', 'Simple Face Average', ...
                'FontSize', 20, ...
                'FontWeight', 'bold', ...
                'Position', [16 78 260 24]);

            app.LoadButton = uibutton(app.HeaderPanel, ...
                'Text', 'Load Image', ...
                'ButtonPushedFcn', @(src, event)app.loadImage(), ...
                'Position', [16 38 100 26]);

            app.DetectButton = uibutton(app.HeaderPanel, ...
                'Text', 'Detect Faces', ...
                'ButtonPushedFcn', @(src, event)app.detectFaces(), ...
                'Position', [124 38 100 26]);

            app.AverageButton = uibutton(app.HeaderPanel, ...
                'Text', 'Create Average Face', ...
                'ButtonPushedFcn', @(src, event)app.createAverageFace(), ...
                'Position', [232 38 146 26]);

            app.CloseButton = uibutton(app.HeaderPanel, ...
                'Text', 'Close', ...
                'ButtonPushedFcn', @(src, event)app.closeApp(), ...
                'Position', [392 38 90 26]);

            app.StatusLabel = uilabel(app.HeaderPanel, ...
                'Text', 'Load an image to begin.', ...
                'Position', [16 12 560 18]);

            app.ContentGrid = uigridlayout(app.MainGrid, [1 2]);
            app.ContentGrid.Layout.Row = 2;
            app.ContentGrid.Layout.Column = 1;
            app.ContentGrid.ColumnWidth = {'1x', '1x'};
            app.ContentGrid.ColumnSpacing = 16;
            app.ContentGrid.Padding = [0 0 0 0];

            app.InputPanel = uipanel(app.ContentGrid, 'Title', 'Input Image');
            app.InputPanel.Layout.Row = 1;
            app.InputPanel.Layout.Column = 1;
            app.InputPanel.BackgroundColor = [1 1 1];

            innerInputGrid = uigridlayout(app.InputPanel, [2 1]);
            innerInputGrid.RowHeight = {'fit', '1x'};
            innerInputGrid.ColumnWidth = {'1x'};
            innerInputGrid.Padding = [12 12 12 12];
            innerInputGrid.RowSpacing = 8;

            app.InstructionLabel = uilabel(innerInputGrid, ...
                'Text', 'Choose an image, detect faces, and build an average.', ...
                'Position', [12 12 420 22]);
            app.InstructionLabel.Layout.Row = 1;
            app.InstructionLabel.Layout.Column = 1;

            app.InputAxes = uiaxes(innerInputGrid);
            app.InputAxes.Layout.Row = 2;
            app.InputAxes.Layout.Column = 1;
            app.InputAxes.XTick = [];
            app.InputAxes.YTick = [];
            app.InputAxes.Box = 'on';

            app.ResultPanel = uipanel(app.ContentGrid, 'Title', 'Average Face');
            app.ResultPanel.Layout.Row = 1;
            app.ResultPanel.Layout.Column = 2;
            app.ResultPanel.BackgroundColor = [1 1 1];

            innerResultGrid = uigridlayout(app.ResultPanel, [3 1]);
            innerResultGrid.RowHeight = {'fit', '1x', 170};
            innerResultGrid.ColumnWidth = {'1x'};
            innerResultGrid.Padding = [12 12 12 12];
            innerResultGrid.RowSpacing = 8;

            resultLabel = uilabel(innerResultGrid, ...
                'Text', 'The average face will appear here after processing.');
            resultLabel.Layout.Row = 1;
            resultLabel.Layout.Column = 1;

            app.ResultAxes = uiaxes(innerResultGrid);
            app.ResultAxes.Layout.Row = 2;
            app.ResultAxes.Layout.Column = 1;
            app.ResultAxes.XTick = [];
            app.ResultAxes.YTick = [];
            app.ResultAxes.Box = 'on';

            app.FaceTable = uitable(innerResultGrid);
            app.FaceTable.Layout.Row = 3;
            app.FaceTable.Layout.Column = 1;
            app.FaceTable.ColumnName = {'Face', 'Include', 'Bounds'};
            app.FaceTable.ColumnEditable = [false true false];
            app.FaceTable.CellEditCallback = @(src, event)app.onFaceTableEdited(event);
            app.FaceTable.Data = app.buildEmptyFaceTable();
            app.ResultAxes.ButtonDownFcn = @(src, event)app.onResultFaceClicked();

            app.showPlaceholder();
        end

        function loadImage(app)
            [fileName, folder] = uigetfile({ '*.jpg;*.jpeg;*.png;*.bmp;*.webp', 'Image files'; '*.*', 'All files' }, ...
                'Select an image file', app.initialFolder);
            if isequal(fileName, 0)
                return;
            end

            fullFileName = fullfile(folder, fileName);
            app.ImageData = app.readImageFile(fullFileName);

            app.FaceBoxes = [];
            app.FaceIncluded = false(0, 1);

            app.showImageOnInputAxes(app.ImageData);
            app.showPlaceholderTable();
            app.StatusLabel.Text = sprintf('Loaded %s', fileName);
        end

        function appFolder = getAppFolder(app)
            classFile = which(class(app));
            if isempty(classFile)
                appFolder = pwd;
                return;
            end

            appFolder = fileparts(classFile);
        end

        function detectFaces(app)
            if isempty(app.ImageData)
                msgbox('Load an image first.', 'No image', 'warn');
                return;
            end

            % Detect candidate face regions using MATLAB's cascade detector.
            detector = vision.CascadeObjectDetector;
            app.FaceBoxes = step(detector, app.ImageData);

            if isempty(app.FaceBoxes)
                msgbox('No faces were detected in the selected image.', 'No faces', 'warn');
                app.StatusLabel.Text = 'No faces detected.';
                app.refreshFaceTable();
                return;
            end

            app.FaceIncluded = true(size(app.FaceBoxes, 1), 1);
            app.refreshFaceTable();

            app.renderDetectedFaces();
            app.StatusLabel.Text = sprintf('Detected %d face(s).', size(app.FaceBoxes, 1));
        end

        function createAverageFace(app)
            if isempty(app.ImageData)
                msgbox('Load an image first.', 'No image', 'warn');
                return;
            end

            if isempty(app.FaceBoxes)
                app.detectFaces();
            end

            if isempty(app.FaceBoxes)
                return;
            end

            includedFaces = find(app.FaceIncluded);
            if isempty(includedFaces)
                msgbox('Enable at least one face in the table before averaging.', 'No faces selected', 'warn');
                app.StatusLabel.Text = 'No faces selected for averaging.';
                return;
            end

            app.updateAverageFacePreview(includedFaces);
            app.StatusLabel.Text = 'Average face created.';
        end

        function updateAverageFacePreview(app, includedFaces)
            if nargin < 2
                includedFaces = find(app.FaceIncluded);
            end

            if isempty(includedFaces)
                app.AveragedImage = uint8([]);
                cla(app.ResultAxes, 'reset');
                placeholderHandle = imshow(zeros(200, 200, 3, 'uint8'), 'Parent', app.ResultAxes);
                placeholderHandle.ButtonDownFcn = @(src, event)app.onResultFaceClicked();
                axis(app.ResultAxes, 'image');
                title(app.ResultAxes, 'Average face');
                return;
            end

            resizedFaces = [];
            % Core algorithm step 1: crop each selected face and normalize size.
            for faceIndex = includedFaces(:)'
                k = faceIndex;
                crop = imcrop(app.ImageData, app.FaceBoxes(k, :));
                crop = imresize(crop, [60 NaN]);
                crop = im2double(crop);
                [irow, icol, ~] = size(crop);
                temp = reshape(crop, irow * icol, 3);
                resizedFaces = cat(3, resizedFaces, temp);
            end

            if isempty(resizedFaces)
                app.StatusLabel.Text = 'No face data available.';
                return;
            end

            % Core algorithm step 2: average all selected faces pixel-wise in RGB.
            meanImage = mean(resizedFaces, 3);
            meanImage = reshape(meanImage, irow, icol, 3);
            meanImage = im2uint8(meanImage);
            meanImage = imresize(meanImage, 10);
            app.AveragedImage = meanImage;

            cla(app.ResultAxes, 'reset');
            averageImageHandle = imshow(meanImage, 'Parent', app.ResultAxes);
            averageImageHandle.ButtonDownFcn = @(src, event)app.onResultFaceClicked();
            axis(app.ResultAxes, 'image');
            title(app.ResultAxes, 'Average face');
        end

        function closeApp(app)
            if isvalid(app.UIFigure)
                delete(app.UIFigure);
            end
        end

        function showImageOnInputAxes(app, imageData)
            cla(app.InputAxes, 'reset');
            imshow(imageData, 'Parent', app.InputAxes);
            axis(app.InputAxes, 'image');
            title(app.InputAxes, 'Input image');
        end

        function renderDetectedFaces(app)
            app.showImageOnInputAxes(app.ImageData);

            if isempty(app.FaceBoxes)
                return;
            end

            hold(app.InputAxes, 'on');
            for k = 1:size(app.FaceBoxes, 1)
                faceBox = app.FaceBoxes(k, :);
                isIncluded = isempty(app.FaceIncluded) || app.FaceIncluded(k);

                if isIncluded
                    edgeColor = [1 0 0];
                    labelBackground = [1 0 0];
                    labelText = [1 1 1];
                else
                    edgeColor = [0.5 0.5 0.5];
                    labelBackground = [0.5 0.5 0.5];
                    labelText = [1 1 1];
                end

                rectangle(app.InputAxes, 'Position', faceBox, ...
                    'EdgeColor', edgeColor, 'LineWidth', 2);

                text(app.InputAxes, faceBox(1), max(faceBox(2) - 8, 1), sprintf('%d', k), ...
                    'Color', labelText, ...
                    'BackgroundColor', labelBackground, ...
                    'FontWeight', 'bold', ...
                    'FontSize', 11, ...
                    'Margin', 1, ...
                    'VerticalAlignment', 'bottom', ...
                    'Clipping', 'on');
            end
            hold(app.InputAxes, 'off');
        end

        function showPlaceholder(app)
            cla(app.InputAxes, 'reset');
            cla(app.ResultAxes, 'reset');
            app.AveragedImage = uint8([]);
            app.LastResultClickTime = NaN;
            imshow(zeros(200, 200, 3, 'uint8'), 'Parent', app.InputAxes);
            axis(app.InputAxes, 'image');
            title(app.InputAxes, 'Input image');
            placeholderHandle = imshow(zeros(200, 200, 3, 'uint8'), 'Parent', app.ResultAxes);
            placeholderHandle.ButtonDownFcn = @(src, event)app.onResultFaceClicked();
            axis(app.ResultAxes, 'image');
            title(app.ResultAxes, 'Average face');
            app.showPlaceholderTable();
        end

        function onResultFaceClicked(app)
            if isempty(app.AveragedImage)
                return;
            end

            currentClickTime = posixtime(datetime('now'));

            if isnan(app.LastResultClickTime)
                app.LastResultClickTime = currentClickTime;
                return;
            end

            elapsedTime = currentClickTime - app.LastResultClickTime;
            app.LastResultClickTime = currentClickTime;

            if elapsedTime > 0.35
                return;
            end

            [saveFileName, saveFolder] = uiputfile({'*.png', 'PNG Image (*.png)'}, ...
                'Export average face as PNG', ...
                fullfile(app.initialFolder, 'average_face.png'));
            if isequal(saveFileName, 0)
                return;
            end

            outputPath = fullfile(saveFolder, saveFileName);
            imwrite(app.AveragedImage, outputPath, 'png');
            app.StatusLabel.Text = sprintf('Saved average face: %s', saveFileName);
            app.LastResultClickTime = NaN;
        end

        function showPlaceholderTable(app)
            app.FaceTable.Data = app.buildEmptyFaceTable();
        end

        function faceTableData = buildEmptyFaceTable(app)
            faceTableData = table(string.empty(0, 1), false(0, 1), string.empty(0, 1), ...
                'VariableNames', {'Face', 'Include', 'Bounds'});
        end

        function imageData = readImageFile(app, fullFileName)
            imageData = imread(fullFileName);

            imageData = app.normalizeImageChannels(imageData);
        end

        function imageData = normalizeImageChannels(~, imageData)
            % Keep downstream processing consistent by always working in RGB.
            if ismatrix(imageData)
                imageData = repmat(imageData, 1, 1, 3);
                return;
            end

            if ndims(imageData) == 3 && size(imageData, 3) == 1
                imageData = repmat(imageData, 1, 1, 3);
                return;
            end

            if ndims(imageData) == 3 && size(imageData, 3) >= 3
                imageData = imageData(:, :, 1:3);
                return;
            end

            error('SimpleFaceAverageApp:UnsupportedImageShape', ...
                'Unsupported image shape %s.', mat2str(size(imageData)));
        end

        function refreshFaceTable(app)
            if isempty(app.FaceBoxes)
                app.FaceTable.Data = app.buildEmptyFaceTable();
                return;
            end

            faceCount = size(app.FaceBoxes, 1);
            if numel(app.FaceIncluded) ~= faceCount
                app.FaceIncluded = true(faceCount, 1);
            end

            faceNames = strings(faceCount, 1);
            faceNames(:) = compose("Face %d", (1:faceCount)');
            boundsText = strings(faceCount, 1);
            for k = 1:faceCount
                boundsText(k) = sprintf('[%d %d %d %d]', ...
                    round(app.FaceBoxes(k, 1)), ...
                    round(app.FaceBoxes(k, 2)), ...
                    round(app.FaceBoxes(k, 3)), ...
                    round(app.FaceBoxes(k, 4)));
            end

            app.FaceTable.Data = table(faceNames, app.FaceIncluded, boundsText, ...
                'VariableNames', {'Face', 'Include', 'Bounds'});
        end

        function onFaceTableEdited(app, event)
            if isempty(app.FaceBoxes)
                return;
            end

            rowIndex = event.Indices(1);
            if rowIndex < 1 || rowIndex > numel(app.FaceIncluded)
                return;
            end

            newValue = event.NewData;
            if islogical(newValue) || isnumeric(newValue)
                app.FaceIncluded(rowIndex) = logical(newValue);
                app.renderDetectedFaces();
                includedFaces = find(app.FaceIncluded);
                app.updateAverageFacePreview(includedFaces);
                app.StatusLabel.Text = sprintf('Face %d include set to %s.', rowIndex, string(app.FaceIncluded(rowIndex)));
            end
        end
    end
end
