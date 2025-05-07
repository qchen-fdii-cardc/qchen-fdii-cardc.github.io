cmake_minimum_required(VERSION 3.25)
project(process_in_rust C)

# Set C standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Add executable
add_executable(process process.c)

# cmake -Bbuild
# cmake --build build
# ./build/process

