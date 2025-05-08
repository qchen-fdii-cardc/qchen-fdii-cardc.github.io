cmake_minimum_required(VERSION 3.10)
project(gcd_example)

# Set C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find Boost package
find_package(Boost REQUIRED COMPONENTS system)
include_directories(${Boost_INCLUDE_DIRS})

# Add executable
add_executable(gcd_example
    main.cpp
)

# Include directories
target_include_directories(gcd_example PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

# Add compiler options
if(MSVC)
    target_compile_options(gcd_example PRIVATE /W4)
else()
    target_compile_options(gcd_example PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Add run target
if(WIN32)
    add_custom_target(run
        COMMAND ${CMAKE_BINARY_DIR}/$<CONFIG>/gcd_example.exe
        DEPENDS gcd_example
        COMMENT "Running gcd_example..."
    )
else()
    add_custom_target(run
        COMMAND ${CMAKE_BINARY_DIR}/gcd_example
        DEPENDS gcd_example
        COMMENT "Running gcd_example..."
    )
endif()

# Usage instructions
message(STATUS "Build commands:")
message(STATUS "  cmake -B build")
message(STATUS "  cmake --build build")
message(STATUS "  cmake --build build --target run")
message(STATUS "")
message(STATUS "Or on Windows with specific config:")
message(STATUS "  cmake -B build")
message(STATUS "  cmake --build build --config Release/Debug")
message(STATUS "  cmake --build build --target run --config Release/Debug")


