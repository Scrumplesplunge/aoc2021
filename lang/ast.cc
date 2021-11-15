#include "ast.h"

#include <iomanip>

namespace aoc2021::ast {
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

}  // namespace

const Location& Expression::location() const {
  return std::visit([](const auto& x) -> const Location& { return x.location; },
                    value_->value);
}

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

std::ostream& operator<<(std::ostream& output, const Name& x) noexcept {
  return output << "Name(" << std::quoted(x.value) << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const IntegerLiteral& x) noexcept {
  return output << "IntegerLiteral(" << x.value << ")";
}

std::ostream& operator<<(std::ostream& output, const Call& x) noexcept {
  return output << "Call(" << x.function << ", " << List(x.arguments) << ")";
}

std::ostream& operator<<(std::ostream& output, const Index& x) noexcept {
  return output << "Index(" << x.container << ", " << x.index << ")";
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

std::ostream& operator<<(std::ostream& output, const Dereference& x) noexcept {
  return output << "Dereference(" << x.inner << ")";
}

std::ostream& operator<<(std::ostream& output, const AddressOf& x) noexcept {
  return output << "AddressOf(" << x.inner << ")";
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

std::ostream& operator<<(std::ostream& output, const GreaterThan& x) noexcept {
  return output << "GreaterThan(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const GreaterOrEqual& x) noexcept {
  return output << "GreaterOrEqual(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const Equal& x) noexcept {
  return output << "Equal(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const NotEqual& x) noexcept {
  return output << "NotEqual(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const LogicalAnd& x) noexcept {
  return output << "LogicalAnd(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const LogicalOr& x) noexcept {
  return output << "LogicalOr(" << x.left << ", " << x.right << ")";
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
                         const TernaryExpression& x) noexcept {
  return output << "TernaryExpression(" << x.condition << ", " << x.then_branch
                << ", " << x.else_branch << ")";
}

std::ostream& operator<<(std::ostream& output, const ArrayType& x) noexcept {
  return output << "ArrayType(" << x.size << ", " << x.element_type << ")";
}

std::ostream& operator<<(std::ostream& output, const SpanType& x) noexcept {
  return output << "SpanType(" << x.element_type << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const Expression& expression) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    expression->value);
}

const Location& Statement::location() const noexcept {
  return std::visit([](const auto& x) -> const Location& { return x.location; },
                    value_->value);
}

const StatementVariant& Statement::operator*() const noexcept {
  return *value_;
}

bool Statement::operator==(const Statement& other) const {
  // Order by pointer value if one is null.
  if (!value_ && !other.value_) return true;
  if (*value_ == *other.value_) return true;
  return false;
}

std::strong_ordering Statement::operator<=>(const Statement& other) const {
  // Order by pointer value if one is null.
  if (!value_ || !other.value_) return value_ <=> other.value_;
  // Otherwise, order by contents.
  return *value_ <=> *other.value_;
}

std::ostream& operator<<(std::ostream& output,
                         const DeclareVariable& x) noexcept {
  return output << "DeclareVariable(" << std::quoted(x.name) << ", " << x.type
                << ")";
}

std::ostream& operator<<(std::ostream& output, const Assign& x) noexcept {
  return output << "Assign(" << x.left << ", " << x.right << ")";
}

std::ostream& operator<<(std::ostream& output, const If& x) noexcept {
  return output << "If(" << x.condition << ", " << List(x.then_branch) << ", "
                << List(x.else_branch) << ")";
}

std::ostream& operator<<(std::ostream& output, const While& x) noexcept {
  return output << "While(" << x.condition << ", " << List(x.body) << ")";
}

std::ostream& operator<<(std::ostream& output, const Return& x) noexcept {
  if (x.value) {
    return output << "Return(" << *x.value << ")";
  } else {
    return output << "Return()";
  }
}

std::ostream& operator<<(std::ostream& output, const Break&) noexcept {
  return output << "Break()";
}

std::ostream& operator<<(std::ostream& output, const Continue&) noexcept {
  return output << "Continue()";
}

std::ostream& operator<<(std::ostream& output,
                         const DiscardedExpression& x) noexcept {
  return output << "DiscardedExpression(" << x.expression << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const FunctionDefinition::Parameter& x) noexcept {
  return output << "Parameter(" << x.name << ", " << x.type << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const FunctionDefinition& x) noexcept {
  return output << "FunctionDefinition(" << std::quoted(x.name) << ", "
                << List(x.parameters) << ", " << x.return_type << ", "
                << List(x.body) << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const Statement& statement) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    statement->value);
}

}  // namespace aoc2021::ast
