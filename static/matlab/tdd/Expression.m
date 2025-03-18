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
                    obj.operands = {};
                case "Variable"
                    obj.value = [];
                    obj.noperands = 0;
                    obj.operands = {};
                case "Function"
                    obj.value = str2func(name);
                    n = numel(varargin);
                    switch n
                        case 0
                            % like e, pi, etc.
                            obj.noperands = 0;
                            obj.operands = {};
                        case 1
                            obj.noperands = varargin{1};
                            obj.operands = {};
                        otherwise
                            obj.noperands = varargin{1};
                            [obj.operands{1:n-1}] = varargin{2:end};
                    end
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
    
    methods % find elements
        function vars = findvars(obj)
            vars = [];
            if obj.isVariable()
                vars = obj;
            elseif obj.isFunction() && ~isempty(obj.operands)
                for i = 1:numel(obj.operands)
                    op_vars = obj.operands{i}.findvars();
                    if ~isempty(op_vars)
                        if isempty(vars)
                            vars = op_vars;
                        else
                            vars = [vars, op_vars];
                        end
                    end
                end
            end
        end
        
        function consts = findconstants(obj)
            consts = [];
            if obj.isConstant()
                consts = obj;
            elseif obj.isFunction() && ~isempty(obj.operands)
                for i = 1:numel(obj.operands)
                    op_consts = obj.operands{i}.findconstants();
                    if ~isempty(op_consts)
                        if isempty(consts)
                            consts = op_consts;
                        else
                            consts = [consts, op_consts];
                        end
                    end
                end
            end
        end
        
        function funcs = findfunctions(obj)
            funcs = [];
            if obj.isFunction()
                funcs = obj;
                if ~isempty(obj.operands)
                    for i = 1:numel(obj.operands)
                        op_funcs = obj.operands{i}.findfunctions();
                        if ~isempty(op_funcs)
                            funcs = [funcs, op_funcs];
                        end
                    end
                end
            end
        end
    end
end