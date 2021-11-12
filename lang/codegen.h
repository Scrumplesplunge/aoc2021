#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "ir.h"

#include <span>
#include <string>

namespace aoc2021 {

std::string Generate(ir::AnyCode code);

}  // namespace aoc2021

#endif  // CODEGEN_H_
