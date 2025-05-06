classdef rs2m
    properties (Constant)
        LIB_PATH = 'rs2m.dll';
        HEADER_PATH = 'rs2m.h';
    end
    methods(Static)
        function ensureLibraryLoaded()
            % Get the current directory
            current_dir = fileparts(mfilename('fullpath'));
            % Load the library
            lib_path = char(fullfile(current_dir, rs2m.LIB_PATH));
            header_path = char(fullfile(current_dir, rs2m.HEADER_PATH));
            [~, ~] = loadlibrary(lib_path, header_path, 'mfilename', 'rs2mproto');
            % Display available functions
            disp('Available library functions:');
            libfunctions('rs2m', '-full')
        end
        
        function unloadLibrary()
            % Unload the library
            if libisloaded('rs2m')
                unloadlibrary('rs2m');
            end
            persistent isLoaded;
            if ~isempty(isLoaded)
                isLoaded = false;
            end
        end
        
        function result = add(a, b)
            % Call Rust add function
            % Parameters:
            %   a, b: uint64 numbers
            % Returns:
            %   result: uint64 result
            if ~isa(a, 'uint64') || ~isa(b, 'uint64')
                error('Inputs must be uint64');
            end
            
            result = calllib('rs2m', 'add', a, b);
        end
        
        function result = linspace(start, end_val, n)
            % Call Rust linspace function
            % Parameters:
            %   start: start value
            %   end_val: end value
            %   n: number of points
            % Returns:
            %   result: array of evenly spaced values
            
            
            % Calculate array length
            len = n;
            
            % Create output array
            result = zeros(1, len);
            ptr = libpointer('doublePtr', result);
            
            % Call Rust function
            try
                actual_len = calllib('rs2m', 'linspace', start, end_val, n, ptr);
                if actual_len ~= len
                    warning('Expected length %d but got %d', len, actual_len);
                end
                result = ptr.Value;
            catch ME
                error('Error calling linspace: %s\nStack: %s', ME.message, getReport(ME, 'extended'));
            end
        end
        
        function result = square(x)
            % Call Rust square function
            % Parameters:
            %   x: input value
            % Returns:
            %   result: x squared
            
            result = calllib('rs2m', 'square', x);
        end
    end
end