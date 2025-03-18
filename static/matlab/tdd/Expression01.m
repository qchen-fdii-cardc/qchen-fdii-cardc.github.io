classdef Expression < handle
    properties
        type
        name
        value
        operands
        noperands
    end
    
    methods
        function obj = Expression(type, name, varargin)
            obj.type = type;
            obj.name = name;
            switch type
                case "Constant"
                    obj.value = varargin{1};
                    obj.noperands = 0;
                    obj.operands = [];
                case "Variable"
                    obj.value = [];
                    obj.noperands = 0;
                    obj.operands = [];
                case "Function"
                    obj.value = str2func(name);
                    obj.noperands = varargin{1};
                    obj.operands = varargin{2:end};
            end
        end
    end
    
    methods % constant
        function isConstant = isConstant(obj)
            isConstant = strcmp(obj.type, "Constant");
        end
        
        function isVariable = isVariable(obj)
            isVariable = strcmp(obj.type, "Variable");
        end
        
        function isFunction = isFunction(obj)
            isFunction = strcmp(obj.type, "Function");
        end
        
    end
end