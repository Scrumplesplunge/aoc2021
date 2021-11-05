#include "parser.h"

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
    std::cout << '{';
    for (const auto& statement : program) {
      std::cout << statement << ',';
    }
    std::cout << "}\n";
  } catch (const aoc2021::ParseError& error) {
    std::cerr << error.what() << '\n';
  }
}
