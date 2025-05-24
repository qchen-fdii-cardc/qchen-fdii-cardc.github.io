addpath("./easy/for_redistribution_files_only/");
dll_lib_name = "easy";
if libisloaded(dll_lib_name)
    unloadlibrary(dll_lib_name);
end

[notfount, warning] = loadlibrary('easy.dll', 'easy.h');


