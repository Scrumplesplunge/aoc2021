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

class CodeFlattener {
 public:
  CodeFlattener(Sequence& result) noexcept : result_(&result) {}
  void operator()(const Sequence& x) {
    for (const auto& y : x.value) std::visit(*this, y->value);
  }
  void operator()(const auto& x) { result_->value.push_back(x); }

 private:
  Sequence* result_;
};

}  // namespace

Label::Label(std::string_view prefix, std::int64_t suffix)
    : value(StrCat("label_", prefix, '_', suffix)) {}

Global::Global(std::string_view prefix, std::int64_t suffix)
    : value(StrCat("global_", prefix, '_', suffix)) {}

const ExpressionVariant& Expression::operator*() const noexcept {
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
                         const Expression& expression) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    expression->value);
}

const CodeVariant& AnyCode::operator*() const noexcept { return *value_; }

std::ostream& operator<<(std::ostream& output, const Store64& x) noexcept {
  return output << "Store64(" << x.address << ", " << x.value << ")";
}

std::ostream& operator<<(std::ostream& output, const StoreCall64& x) noexcept {
  return output << "StoreCall64(" << x.result_address << ", "
                << x.function_address << ", " << List(x.arguments) << ")";
}

std::ostream& operator<<(std::ostream& output, const BeginFrame& x) noexcept {
  return output << "BeginFrame(" << x.size << ")";
}

std::ostream& operator<<(std::ostream& output, const Return& x) noexcept {
  return output << "Return(" << x.value << ")";
}

std::ostream& operator<<(std::ostream& output, const Jump& x) noexcept {
  return output << "Jump(Label(" << std::quoted(x.target.value) << "))";
}

std::ostream& operator<<(std::ostream& output, const JumpIf& x) noexcept {
  return output << "JumpIf(" << x.condition << ", Label("
                << std::quoted(x.target.value) << "))";
}

std::ostream& operator<<(std::ostream& output, const JumpUnless& x) noexcept {
  return output << "JumpUnless(" << x.condition << ", Label("
                << std::quoted(x.target.value) << "))";
}

std::ostream& operator<<(std::ostream& output, const Sequence& x) noexcept {
  return output << "Sequence(" << List(x.value) << ")";
}

std::ostream& operator<<(std::ostream& output, const AnyCode& code) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    code->value);
}

Sequence Flatten(const AnyCode& code) {
  Sequence sequence;
  std::visit(CodeFlattener(sequence), code->value);
  return sequence;
}

}  // namespace aoc2021::ir
