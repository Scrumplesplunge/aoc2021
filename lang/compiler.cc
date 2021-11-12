#include "parser.h"
#include "checker.h"
#include "codegen.h"

#include <exception>
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: compiler <filename>\n";
    return 1;
  }
  aoc2021::Source source(argv[1]);
  aoc2021::Parser parser(source);
  try {
    const auto program = parser.ParseProgram();
    std::cerr << "Check({";
    for (const auto& statement : program) {
      std::cerr << statement << ',';
    }
    std::cerr << "});\n";
    const auto code = aoc2021::Check(program);
    std::cerr << '\n' << code << '\n';
    std::cout << aoc2021::Generate(code) << '\n';
  } catch (const aoc2021::ParseError& error) {
    std::cerr << error.what() << '\n';
  }
}
