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
  void operator()(const BeginFrame& x) override {
    *output_ << "BeginFrame(" << x.size << ")";
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
  void operator()(const BeginFrame& x) override { result_->value.push_back(x); }
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

const ExpressionVariant& AnyExpression::operator*() const noexcept {
  return *value_;
}

std::ostream& operator<<(std::ostream& output, const Label& x) noexcept {
  return output << "Label(" << std::quoted(x.value) << ")";
}

std::ostream& operator<<(std::ostream& output, const Global& x) noexcept {
  return output << "Global(" << std::quoted(x.value) << ")";
}

std::ostream& operator<<(std::ostream& output, const Local& x) noexcept {
  return output << "Local(Offset{" << static_cast<std::int64_t>(x.offset) << "})";
}

std::ostream& operator<<(std::ostream& output, const Load64& x) noexcept {
  return output << "Load64(" << x.address << ")";
}

std::ostream& operator<<(std::ostream& output, const IntegerLiteral& x) noexcept {
  return output << "IntegerLiteral(" << x.value << ")";
}

std::ostream& operator<<(std::ostream& output, const Negate& x) noexcept {
  return output << "Negate(" << x.inner << ")";
}

std::ostream& operator<<(std::ostream& output, const LogicalNot& x) noexcept {
  return output << "LogicalNot(" << x.inner << ")";
}

std::ostream& operator<<(std::ostream& output, const BitwiseNot& x) noexcept {
  return output << "BitwiseNot(" << x.inner << ")";
}

std::ostream& operator<<(std::ostream& output, const Add& x) noexcept {
  return output << "Add(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const Subtract& x) noexcept {
  return output << "Subtract(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const Multiply& x) noexcept {
  return output << "Multiply(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const Divide& x) noexcept {
  return output << "Divide(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const Modulo& x) noexcept {
  return output << "Modulo(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const LessThan& x) noexcept {
  return output << "LessThan(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const LessOrEqual& x) noexcept {
  return output << "LessOrEqual(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const Equal& x) noexcept {
  return output << "Equal(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const NotEqual& x) noexcept {
  return output << "NotEqual(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const BitwiseAnd& x) noexcept {
  return output << "BitwiseAnd(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const BitwiseOr& x) noexcept {
  return output << "BitwiseOr(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const BitwiseXor& x) noexcept {
  return output << "BitwiseXor(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const ShiftLeft& x) noexcept {
  return output << "ShiftLeft(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const ShiftRight& x) noexcept {
  return output << "ShiftRight(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const AnyExpression& expression) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    expression->value);
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
