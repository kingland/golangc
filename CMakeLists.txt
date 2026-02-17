cmake_minimum_required(VERSION 3.25)
project(golang-compiler VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# MSVC-specific flags
if(MSVC)
    add_compile_options(
        /W4 /WX           # Warnings as errors
        /permissive-      # Strict conformance
        /Zc:__cplusplus   # Correct __cplusplus macro
        /utf-8            # UTF-8 source and execution charset
        /EHsc             # Exception handling model
    )
endif()

# Export compile commands for IDE support
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Dependencies
find_package(fmt CONFIG REQUIRED)
find_package(GTest CONFIG REQUIRED)

# Enable testing
include(CTest)
enable_testing()

# Subdirectories
add_subdirectory(src)
add_subdirectory(tests)
