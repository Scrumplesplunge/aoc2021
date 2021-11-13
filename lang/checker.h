#ifndef CHECKER_H_
#define CHECKER_H_

#include "ast.h"
#include "ir.h"
#include "source.h"

#include <span>

namespace aoc2021 {

struct CheckError : public SourceError {
  using SourceError::SourceError;
};

ir::Unit Check(std::span<const ast::AnyStatement> program);

}  // namespace aoc2021

#endif  // CHECKER_H_
