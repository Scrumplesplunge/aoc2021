#include "ir.h"

#include <iomanip>

namespace aoc2021::ir {
namespace {

class StepPrinter : public StepVisitor<void> {
 public:
  StepPrinter(std::ostream& output) noexcept : output_(&output) {}
  void operator()(const Global& x) override {
    *output_ << "Global(" << std::quoted(x.name) << ")";
  }
  void operator()(const Local& x) override {
    *output_ << "Local(" << x.offset << ")";
  }
  void operator()(const Load64& x) override { *output_ << "Load64()"; }
  void operator()(const Store64& x) override { *output_ << "Store64()"; }
  void operator()(const IntegerLiteral& x) override {
    *output_ << "IntegerLiteral(" << x.value << ")";
  }
  void operator()(const Call& x) override { *output_ << "Call()"; }
  void operator()(const Negate& x) override { *output_ << "Negate()"; }
  void operator()(const LogicalNot& x) override { *output_ << "LogicalNot()"; }
  void operator()(const BitwiseNot& x) override { *output_ << "BitwiseNot()"; }
  void operator()(const Add& x) override { *output_ << "Add()"; }
  void operator()(const Subtract& x) override { *output_ << "Subtract()"; }
  void operator()(const Multiply& x) override { *output_ << "Multiply()"; }
  void operator()(const Divide& x) override { *output_ << "Divide()"; }
  void operator()(const Modulo& x) override { *output_ << "Modulo()"; }
  void operator()(const LessThan& x) override { *output_ << "LessThan()"; }
  void operator()(const LessOrEqual& x) override {
    *output_ << "LessOrEqual()";
  }
  void operator()(const Equal& x) override { *output_ << "Equal()"; }
  void operator()(const NotEqual& x) override { *output_ << "NotEqual()"; }
  void operator()(const BitwiseAnd& x) override { *output_ << "BitwiseAnd()"; }
  void operator()(const BitwiseOr& x) override { *output_ << "BitwiseOr()"; }
  void operator()(const BitwiseXor& x) override { *output_ << "BitwiseXor()"; }
  void operator()(const ShiftLeft& x) override { *output_ << "ShiftLeft()"; }
  void operator()(const ShiftRight& x) override { *output_ << "ShiftRight()"; }

 private:
  std::ostream* output_;
};

}  // namespace

std::ostream& operator<<(std::ostream& output, const AnyStep& step) noexcept {
  StepPrinter printer(output);
  step.Visit(printer);
  return output;
}

}  // namespace aoc2021::ir
