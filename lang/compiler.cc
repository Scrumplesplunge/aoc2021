#include "parser.h"
#include "checker.h"
#include "codegen.h"

#include <cassert>
#include <exception>
#include <iostream>

enum class OutputType {
  kAst,
  kIr,
  kAssembly,
};

struct {
  OutputType output_type = OutputType::kAssembly;
} options;

void ParseFlags(int& argc, char**& argv) {
  bool skip_options = false;
  int j = 0;
  for (int i = 0; i < argc; i++) {
    std::string_view value = argv[i];
    if (value == "--") skip_options = true;
    if (skip_options || !value.starts_with("--")) {
      argv[j++] = argv[i];
      continue;
    }
    if (value == "--ast") {
      options.output_type = OutputType::kAst;
    } else if (value == "--ir") {
      options.output_type = OutputType::kIr;
    } else if (value == "--assembly") {
      options.output_type = OutputType::kAssembly;
    } else {
      std::cerr << "Unrecognised command-line option " << value << "\n";
      std::exit(EXIT_FAILURE);
    }
  }
  argc = j;
}

int main(int argc, char* argv[]) {
  ParseFlags(argc, argv);
  if (argc != 2) {
    std::cerr << "Usage: compiler [options] <filename>\n";
    return EXIT_FAILURE;
  }
  aoc2021::Source source(argv[1]);
  aoc2021::Parser parser(source);
  try {
    if (options.output_type == OutputType::kAst) {
      const auto program = parser.ParseProgram();
      // Print the statements as an initializer list.
      for (const auto& statement : program) {
        std::cout << statement << ";\n";
      }
      return EXIT_SUCCESS;
    }
    aoc2021::Checker checker;
    checker.Check(argv[1]);
    const aoc2021::ir::Unit unit = checker.Finish();
    if (options.output_type == OutputType::kIr) {
      std::cout << unit.code << '\n';
      return EXIT_SUCCESS;
    }
    if (!unit.main) {
      std::cerr << "Program does not contain a main function.\n";
      return EXIT_FAILURE;
    }
    assert(options.output_type == OutputType::kAssembly);
    std::cout << aoc2021::Generate(unit) << '\n';
  } catch (const aoc2021::CheckError& error) {
    std::cerr << error.what() << '\n';
    return 1;
  }
}
