#include "ir.h"

#include "string_utils.h"

#include <iomanip>

namespace aoc2021::ir {
namespace {

template <typename T>
struct List {
  explicit List(const T& value) : value(value) {}

  const T& value;

  friend inline std::ostream& operator<<(std::ostream& output, List list) {
    output << "{";
    bool first = true;
    for (const auto& x : list.value) {
      if (first) {
        first = false;
      } else {
        output << ", ";
      }
      output << x;
    }
    return output << "}";
  }
};

template <typename T> List(T) -> List<T>;

class ExpressionPrinter : public ExpressionVisitor<void> {
 public:
  ExpressionPrinter(std::ostream& output) noexcept : output_(&output) {}
  void operator()(const Label& x) override {
    *output_ << "Label(" << std::quoted(x.value) << ")";
  }
  void operator()(const Global& x) override {
    *output_ << "Global(" << std::quoted(x.value) << ")";
  }
  void operator()(const Local& x) override {
    *output_ << "Local(Offset{" << static_cast<std::int64_t>(x.offset) << "})";
  }
  void operator()(const Load64& x) override {
    *output_ << "Load64(" << x.address << ")";
  }
  void operator()(const IntegerLiteral& x) override {
    *output_ << "IntegerLiteral(" << x.value << ")";
  }
  void operator()(const Negate& x) override {
    *output_ << "Negate(" << x.inner << ")";
  }
  void operator()(const LogicalNot& x) override {
    *output_ << "LogicalNot(" << x.inner << ")";
  }
  void operator()(const BitwiseNot& x) override {
    *output_ << "BitwiseNot(" << x.inner << ")";
  }
  void operator()(const Add& x) override {
    *output_ << "Add(" << x.left << ", " << x.right << ")";
  }
  void operator()(const Subtract& x) override {
    *output_ << "Subtract(" << x.left << ", " << x.right << ")";
  }
  void operator()(const Multiply& x) override {
    *output_ << "Multiply(" << x.left << ", " << x.right << ");";
  }
  void operator()(const Divide& x) override {
    *output_ << "Divide(" << x.left << ", " << x.right << ")";
  }
  void operator()(const Modulo& x) override {
    *output_ << "Modulo(" << x.left << ", " << x.right << ")";
  }
  void operator()(const LessThan& x) override {
    *output_ << "LessThan(" << x.left << ", " << x.right << ")";
  }
  void operator()(const LessOrEqual& x) override {
    *output_ << "LessOrEqual(" << x.left << ", " << x.right << ")";
  }
  void operator()(const Equal& x) override {
    *output_ << "Equal(" << x.left << ", " << x.right << ")";
  }
  void operator()(const NotEqual& x) override {
    *output_ << "NotEqual(" << x.left << ", " << x.right << ")";
  }
  void operator()(const BitwiseAnd& x) override {
    *output_ << "BitwiseAnd(" << x.left << ", " << x.right << ")";
  }
  void operator()(const BitwiseOr& x) override {
    *output_ << "BitwiseOr(" << x.left << ", " << x.right << ")";
  }
  void operator()(const BitwiseXor& x) override {
    *output_ << "BitwiseXor(" << x.left << ", " << x.right << ")";
  }
  void operator()(const ShiftLeft& x) override {
    *output_ << "ShiftLeft(" << x.left << ", " << x.right << ")";
  }
  void operator()(const ShiftRight& x) override {
    *output_ << "ShiftRight(" << x.left << ", " << x.right << ")";
  }

 private:
  std::ostream* output_;
};

class CodePrinter : public CodeVisitor<void> {
 public:
  CodePrinter(std::ostream& output) noexcept : output_(&output) {}
  void operator()(const Label& x) override {
    *output_ << "Label(" << std::quoted(x.value) << ")";
  }
  void operator()(const Store64& x) override {
    *output_ << "Store64(" << x.address << ", " << x.value << ")";
  }
  void operator()(const StoreCall64& x) override {
    *output_ << "StoreCall64(" << x.result_address << ", " << x.function_address
             << ", " << List(x.arguments) << ")";
  }
  void operator()(const AdjustStack& x) override {
    *output_ << "AdjustStack(" << x.delta << ")";
  }
  void operator()(const Return& x) override {
    *output_ << "Return(" << x.value << ")";
  }
  void operator()(const Jump& x) override {
    *output_ << "Jump(Label(" << std::quoted(x.target.value) << "))";
  }
  void operator()(const JumpIf& x) override {
    *output_ << "JumpIf(" << x.condition << ", Label("
             << std::quoted(x.target.value) << "))";
  }
  void operator()(const JumpUnless& x) override {
    *output_ << "JumpUnless(" << x.condition << ", Label("
             << std::quoted(x.target.value) << "))";
  }
  void operator()(const Sequence& x) override {
    *output_ << "Sequence(" << List(x.value) << ")";
  }

 private:
  std::ostream* output_;
};

class CodeFlattener : public CodeVisitor<void> {
 public:
  CodeFlattener(Sequence& result) noexcept : result_(&result) {}
  void operator()(const Label& x) override { result_->value.push_back(x); }
  void operator()(const Store64& x) override { result_->value.push_back(x); }
  void operator()(const StoreCall64& x) override {
    result_->value.push_back(x);
  }
  void operator()(const AdjustStack& x) override { result_->value.push_back(x); }
  void operator()(const Return& x) override { result_->value.push_back(x); }
  void operator()(const Jump& x) override { result_->value.push_back(x); }
  void operator()(const JumpIf& x) override { result_->value.push_back(x); }
  void operator()(const JumpUnless& x) override { result_->value.push_back(x); }
  void operator()(const Sequence& x) override {
    for (const auto& y : x.value) y.Visit(*this);
  }

 private:
  Sequence* result_;
};

}  // namespace

std::ostream& operator<<(std::ostream& output,
                         const AnyExpression& step) noexcept {
  ExpressionPrinter printer(output);
  step.Visit(printer);
  return output;
}

Label::Label(std::string_view prefix, std::int64_t suffix)
    : value(StrCat("label_", prefix, '_', suffix)) {}

Global::Global(std::string_view prefix, std::int64_t suffix)
    : value(StrCat("global_", prefix, '_', suffix)) {}

std::ostream& operator<<(std::ostream& output,
                         const AnyCode& step) noexcept {
  CodePrinter printer(output);
  step.Visit(printer);
  return output;
}

Sequence Flatten(const AnyCode& code) {
  Sequence sequence;
  CodeFlattener flatten(sequence);
  code.Visit(flatten);
  return sequence;
}

}  // namespace aoc2021::ir
