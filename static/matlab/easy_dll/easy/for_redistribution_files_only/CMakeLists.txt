cmake_minimum_required(VERSION 3.10)
project(matlab_dll_demo C)

# 设置C标准
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# 设置MATLAB安装路径
set(MATLAB_ROOT "C:/Program Files/MATLAB/R2023b")
set(MATLAB_INCLUDE_DIRS 
    "${MATLAB_ROOT}/extern/include"
    "${MATLAB_ROOT}/sys/opengl/include"
    "${MATLAB_ROOT}/sys/os/glnxa64"
)
set(MATLAB_LIBRARY_DIRS "${MATLAB_ROOT}/extern/lib/win64/microsoft")

# 设置Release版本的编译选项
if(MSVC)
    set(CMAKE_C_FLAGS_RELEASE "/O2 /DNDEBUG")
else()
    set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG")
endif()

# 添加可执行文件
add_executable(${PROJECT_NAME} main.c easy.lib)

# 添加头文件目录
target_include_directories(${PROJECT_NAME} PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${MATLAB_INCLUDE_DIRS}
)

# 添加库文件目录
link_directories(${MATLAB_LIBRARY_DIRS})

# 链接MATLAB DLL和运行时库
target_link_libraries(${PROJECT_NAME} PRIVATE 
    ${CMAKE_CURRENT_SOURCE_DIR}/easy.lib
    "${MATLAB_LIBRARY_DIRS}/libmex.lib"
    "${MATLAB_LIBRARY_DIRS}/libmx.lib"
    "${MATLAB_LIBRARY_DIRS}/libmat.lib"
    "${MATLAB_LIBRARY_DIRS}/mclmcrrt.lib"
)

# 复制DLL到输出目录
add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${CMAKE_CURRENT_SOURCE_DIR}/easy.dll
    $<TARGET_FILE_DIR:${PROJECT_NAME}>) 