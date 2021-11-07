#ifndef CHECKER_H_
#define CHECKER_H_

#include "ast.h"
#include "source.h"

#include <span>

namespace aoc2021 {

struct CheckError : public SourceError {
  using SourceError::SourceError;
};

// Eventually, the checker will emit IR as it checks the program. For now, it
// just checks the correctness of the input.
struct TODO {};

TODO Check(std::span<const ast::AnyStatement> program);

}  // namespace aoc2021

#endif  // CHECKER_H_
