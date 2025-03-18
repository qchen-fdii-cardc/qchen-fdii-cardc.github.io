% 测试表达式类

%% Test 1: Constant
c1 = Expression("Constant", "c1", 1);
assert(isa(c1, 'handle'));
assert(c1.isConstant());
assert(~c1.isVariable());
assert(~c1.isFunction());
assert(c1.value == 1);
assert(strcmp(c1.name, "c1"));
assert(isempty(c1.operands));


%% Test 2: Variable
x1 = Expression("Variable", "x1");
assert(isa(x1, 'handle'));
assert(~x1.isConstant());
assert(x1.isVariable());
assert(~x1.isFunction());
assert(strcmp(x1.name, "x1"));
assert(isempty(x1.value));
assert(isempty(x1.operands));



%% test 3: Function
f1 = Expression("Function", "plus", 2);
assert(isa(f1, 'handle'));
assert(f1.isFunction());
assert(~f1.isConstant());
assert(~f1.isVariable());
assert(strcmp(f1.name, "plus"));
assert(f1.noperands == 2);

%% Test 4: Function with operands
c1 = Expression("Constant", "c1", 1);
x1 = Expression("Variable", "x1");
f2 = Expression("Function", "plus", 2, x1, c1);
assert(f2.noperands == 2);
assert(f2.operands{1} == x1);
assert(f2.operands{2} == c1);



%% Test 4: find all variables
vars = f1.findvars();
assert(isequal(vars, x1));

%% Test 5: find all constants
consts = f1.findconstants();
assert(isequal(consts, c1));

%% Test 6: find all functions
funcs = f1.findfunctions();
assert(isequal(funcs, f1));


































