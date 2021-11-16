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

Label::Label(std::string_view name) noexcept : value(name) {}

Label::Label(std::string_view prefix, std::int64_t suffix) noexcept
    : value(StrCat("label_", prefix, '_', suffix)) {}

Global::Global(std::string_view prefix, std::int64_t suffix)
    : value(StrCat("global_", prefix, '_', suffix)) {}

const ExpressionVariant& Expression::operator*() const noexcept {
  return *value_;
}

bool Expression::operator==(const Expression& other) const {
  // Order by pointer value if one is null.
  if (!value_ && !other.value_) return true;
  if (*value_ == *other.value_) return true;
  return false;
}

std::strong_ordering Expression::operator<=>(const Expression& other) const {
  // Order by pointer value if one is null.
  if (!value_ || !other.value_) return value_ <=> other.value_;
  // Otherwise, order by contents.
  return *value_ <=> *other.value_;
}

std::ostream& operator<<(std::ostream& output, const Label& x) noexcept {
  return output << "Label(" << Escaped(x.value) << ")";
}

std::ostream& operator<<(std::ostream& output, const Global& x) noexcept {
  return output << "Global(" << Escaped(x.value) << ")";
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

const TypeVariant& Type::operator*() const noexcept { return *value_; }

bool Type::operator==(const Type& other) const {
  // Order by pointer value if one is null.
  if (!value_ && !other.value_) return true;
  if (*value_ == *other.value_) return true;
  return false;
}

std::strong_ordering Type::operator<=>(const Type& other) const {
  // Order by pointer value if one is null.
  if (!value_ || !other.value_) return value_ <=> other.value_;
  // Otherwise, order by contents.
  return *value_ <=> *other.value_;
}

std::ostream& operator<<(std::ostream& output, Primitive x) noexcept {
  switch (x) {
    case Primitive::kVoid: return output << "Primitive::kVoid";
    case Primitive::kByte: return output << "Primitive::kByte";
    case Primitive::kInt64: return output << "Primitive::kInt64";
  }
  std::abort();
}

std::ostream& operator<<(std::ostream& output, const Pointer& x) noexcept {
  return output << "Pointer(" << x.pointee << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const FunctionPointer& x) noexcept {
  return output << "FunctionPointer(" << x.return_type << ", "
                << List(x.parameters) << ")";
}

std::ostream& operator<<(std::ostream& output, const Array& x) noexcept {
  return output << "Array(" << x.size << ", " << x.element << ")";
}

std::ostream& operator<<(std::ostream& output, const Span& x) noexcept {
  return output << "Span(" << x.element << ")";
}

std::ostream& operator<<(std::ostream& output, const Type& x) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    x->value);
}

std::int64_t Size(Primitive x) noexcept {
  switch (x) {
    case Primitive::kVoid: return 0;
    case Primitive::kByte: return 1;
    case Primitive::kInt64: return 8;
  }
  std::abort();
}

std::int64_t Size(const Pointer& x) noexcept { return 8; }
std::int64_t Size(const FunctionPointer& x) noexcept { return 8; }

std::int64_t Size(const Array& x) noexcept {
  return x.size * Size(x.element);
}

std::int64_t Size(const Span& x) noexcept { return 8; }

std::int64_t Size(const Type& x) noexcept {
  return std::visit([](const auto& x) { return Size(x); }, x->value);
}

std::int64_t Alignment(Primitive x) noexcept {
  switch (x) {
    case Primitive::kVoid: return 1;
    case Primitive::kByte: return 1;
    case Primitive::kInt64: return 8;
  }
  std::abort();
}

std::int64_t Alignment(const Pointer& x) noexcept { return 8; }
std::int64_t Alignment(const FunctionPointer& x) noexcept { return 8; }

std::int64_t Alignment(const Array& x) noexcept {
  return Alignment(x.element);
}

std::int64_t Alignment(const Span& x) noexcept { return 8; }

std::int64_t Alignment(const Type& x) noexcept {
  return std::visit([](const auto& x) { return Alignment(x); }, x->value);
}

const CodeVariant& Code::operator*() const noexcept { return *value_; }

bool Code::operator==(const Code& other) const {
  // Order by pointer value if one is null.
  if (!value_ && !other.value_) return true;
  if (*value_ == *other.value_) return true;
  return false;
}

std::strong_ordering Code::operator<=>(const Code& other) const {
  // Order by pointer value if one is null.
  if (!value_ || !other.value_) return value_ <=> other.value_;
  // Otherwise, order by contents.
  return *value_ <=> *other.value_;
}

std::ostream& operator<<(std::ostream& output, const Store8& x) noexcept {
  return output << "Store8(" << x.address << ", " << x.value << ")";
}

std::ostream& operator<<(std::ostream& output, const Store64& x) noexcept {
  return output << "Store64(" << x.address << ", " << x.value << ")";
}

std::ostream& operator<<(std::ostream& output, const Call& x) noexcept {
  return output << "Call(" << x.function_address << ", " << List(x.arguments)
                << ")";
}

std::ostream& operator<<(std::ostream& output, const BeginFrame& x) noexcept {
  return output << "BeginFrame(" << x.size << ")";
}

std::ostream& operator<<(std::ostream& output, const Return& x) noexcept {
  return output << "Return()";
}

std::ostream& operator<<(std::ostream& output, const Jump& x) noexcept {
  return output << "Jump(Label(" << Escaped(x.target.value) << "))";
}

std::ostream& operator<<(std::ostream& output, const JumpIf& x) noexcept {
  return output << "JumpIf(" << x.condition << ", Label("
                << Escaped(x.target.value) << "))";
}

std::ostream& operator<<(std::ostream& output, const JumpUnless& x) noexcept {
  return output << "JumpUnless(" << x.condition << ", Label("
                << Escaped(x.target.value) << "))";
}

std::ostream& operator<<(std::ostream& output, const Sequence& x) noexcept {
  return output << "Sequence(" << List(x.value) << ")";
}

std::ostream& operator<<(std::ostream& output, const Code& code) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    code->value);
}

Sequence Flatten(const Code& code) {
  Sequence sequence;
  std::visit(CodeFlattener(sequence), code->value);
  return sequence;
}

}  // namespace aoc2021::ir
