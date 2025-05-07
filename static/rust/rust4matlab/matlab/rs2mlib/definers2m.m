%% About definers2m.m
% This file defines the MATLAB interface to the library |rs2m|.
%
% Commented sections represent C++ functionality that MATLAB cannot automatically define. To include
% functionality, uncomment a section and provide values for <SHAPE>, <DIRECTION>, etc. For more
% information, see helpview(fullfile(docroot,'matlab','helptargets.map'),'cpp_define_interface') to "Define MATLAB Interface for C++ Library".



%% Setup
% Do not edit this setup section.
function libDef = definers2m()
libDef = clibgen.LibraryDefinition("rs2mData.xml");

%% OutputFolder and Libraries
libDef.OutputFolder = "D:\writing\qchen-fdii-cardc.github.io\static\rust\rust4matlab\matlab\rs2mlib";
libDef.Libraries = [ "rs2m.dll" "rs2m.lib" ];

%% C++ function |add| with MATLAB name |clib.rs2m.add|
% C++ Signature: uint64_t add(uint64_t left,uint64_t right)

addDefinition = addFunction(libDef, ...
    "uint64_t add(uint64_t left,uint64_t right)", ...
    "MATLABName", "clib.rs2m.add", ...
    "Description", "clib.rs2m.add Representation of C++ function add."); % Modify help description values as needed.
defineArgument(addDefinition, "left", "uint64");
defineArgument(addDefinition, "right", "uint64");
defineOutput(addDefinition, "RetVal", "uint64");
validate(addDefinition);

%% C++ function |linspace| with MATLAB name |clib.rs2m.linspace|
% C++ Signature: int32_t linspace(double start,double end,int32_t n,double * out_ptr)

linspaceDefinition = addFunction(libDef, ...
    "int32_t linspace(double start,double end,int32_t n,double * out_ptr)", ...
    "MATLABName", "clib.rs2m.linspace", ...
    "Description", "clib.rs2m.linspace Representation of C++ function linspace."); % Modify help description values as needed.
defineArgument(linspaceDefinition, "start", "double");
defineArgument(linspaceDefinition, "end", "double");
defineArgument(linspaceDefinition, "n", "int32");
defineArgument(linspaceDefinition, "out_ptr", "clib.array.rs2m.Double", "input", "n"); % <MLTYPE> can be "clib.array.rs2m.Double", or "double"
defineOutput(linspaceDefinition, "RetVal", "int32");
validate(linspaceDefinition);

%% C++ function |square| with MATLAB name |clib.rs2m.square|
% C++ Signature: double square(double x)

squareDefinition = addFunction(libDef, ...
    "double square(double x)", ...
    "MATLABName", "clib.rs2m.square", ...
    "Description", "clib.rs2m.square Representation of C++ function square."); % Modify help description values as needed.
defineArgument(squareDefinition, "x", "double");
defineOutput(squareDefinition, "RetVal", "double");
validate(squareDefinition);

%% Validate the library definition
validate(libDef);

end
