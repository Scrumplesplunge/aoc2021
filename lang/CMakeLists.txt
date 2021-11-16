cmake_minimum_required(VERSION 3.1)

project(aoc2021)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_compile_options(-Werror -Wall -Wextra -Wno-unused-parameter -pedantic -g3)

add_library(string_utils string_utils.cc string_utils.h)

add_library(test test.cc test.h)
target_link_libraries(test string_utils)

add_library(source source.cc source.h)

add_library(ast ast.cc ast.h)
target_link_libraries(ast source)
add_executable(source_test source_test.cc)
target_link_libraries(source_test source test)

add_library(parser parser.cc parser.h)
target_link_libraries(parser source ast)
add_executable(parser_test parser_test.cc)
target_link_libraries(parser_test parser test)

add_library(ir ir.cc ir.h)
target_link_libraries(ir string_utils)

add_library(checker checker.cc checker.h)
target_link_libraries(checker source ast ir string_utils)
add_executable(checker_test checker_test.cc)
target_link_libraries(checker_test checker parser test)

add_library(codegen codegen.cc codegen.h)
target_link_libraries(codegen ir)

add_executable(compiler compiler.cc)
target_link_libraries(compiler parser checker codegen)
