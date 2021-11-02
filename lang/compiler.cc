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
    std::cout << parser.ParseExpression() << '\n';
  } catch (const aoc2021::ParseError& error) {
    std::cerr << error.what() << '\n';
  }
}
