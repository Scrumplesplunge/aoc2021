cmake_minimum_required(VERSION 3.1)

project(aoc2021)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_compile_options(-Werror -Wall -Wextra -Wno-unused-parameter -pedantic -g3)

add_executable(compiler compiler.cc ast.cc checker.cc source.cc parser.cc)
