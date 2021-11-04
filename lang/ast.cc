#include "ast.h"

#include <iomanip>

namespace aoc2021 {
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

std::ostream& operator<<(std::ostream& output,
                         const AnyExpression& expression) noexcept {
  expression.Print(output);
  return output;
}

void Name::Print(std::ostream& output) const noexcept {
  output << "Name(" << std::quoted(value_) << ")";
}

void IntegerLiteral::Print(std::ostream& output) const noexcept {
  output << "IntegerLiteral(" << value_ << ")";
}

void Call::Print(std::ostream& output) const noexcept {
  output << "Call(" << function_ << ", " << List(arguments_) << ")";
}

void Index::Print(std::ostream& output) const noexcept {
  output << "Index(" << container_ << ", " << index_ << ")";
}

void Negate::Print(std::ostream& output) const noexcept {
  output << "Negate(" << inner_ << ")";
}

void LogicalNot::Print(std::ostream& output) const noexcept {
  output << "LogicalNot(" << inner_ << ")";
}

void BitwiseNot::Print(std::ostream& output) const noexcept {
  output << "BitwiseNot(" << inner_ << ")";
}

void Dereference::Print(std::ostream& output) const noexcept {
  output << "Dereference(" << inner_ << ")";
}

void Add::Print(std::ostream& output) const noexcept {
  output << "Add(" << left_ << ", " << right_ << ")";
}

void Subtract::Print(std::ostream& output) const noexcept {
  output << "Subtract(" << left_ << ", " << right_ << ")";
}

void Multiply::Print(std::ostream& output) const noexcept {
  output << "Multiply(" << left_ << ", " << right_ << ")";
}

void Divide::Print(std::ostream& output) const noexcept {
  output << "Divide(" << left_ << ", " << right_ << ")";
}

void Modulo::Print(std::ostream& output) const noexcept {
  output << "Modulo(" << left_ << ", " << right_ << ")";
}

void LessThan::Print(std::ostream& output) const noexcept {
  output << "LessThan(" << left_ << ", " << right_ << ")";
}

void LessOrEqual::Print(std::ostream& output) const noexcept {
  output << "LessOrEqual(" << left_ << ", " << right_ << ")";
}

void GreaterThan::Print(std::ostream& output) const noexcept {
  output << "GreaterThan(" << left_ << ", " << right_ << ")";
}

void GreaterOrEqual::Print(std::ostream& output) const noexcept {
  output << "GreaterOrEqual(" << left_ << ", " << right_ << ")";
}

void Equal::Print(std::ostream& output) const noexcept {
  output << "Equal(" << left_ << ", " << right_ << ")";
}

void NotEqual::Print(std::ostream& output) const noexcept {
  output << "NotEqual(" << left_ << ", " << right_ << ")";
}

void LogicalAnd::Print(std::ostream& output) const noexcept {
  output << "LogicalAnd(" << left_ << ", " << right_ << ")";
}

void LogicalOr::Print(std::ostream& output) const noexcept {
  output << "LogicalOr(" << left_ << ", " << right_ << ")";
}

void BitwiseAnd::Print(std::ostream& output) const noexcept {
  output << "BitwiseAnd(" << left_ << ", " << right_ << ")";
}

void BitwiseOr::Print(std::ostream& output) const noexcept {
  output << "BitwiseOr(" << left_ << ", " << right_ << ")";
}

void BitwiseXor::Print(std::ostream& output) const noexcept {
  output << "BitwiseXor(" << left_ << ", " << right_ << ")";
}

void ShiftLeft::Print(std::ostream& output) const noexcept {
  output << "ShiftLeft(" << left_ << ", " << right_ << ")";
}

void ShiftRight::Print(std::ostream& output) const noexcept {
  output << "ShiftRight(" << left_ << ", " << right_ << ")";
}

void TernaryExpression::Print(std::ostream& output) const noexcept {
  output << "TernaryExpression(" << condition_ << ", " << then_branch_ << ", "
         << else_branch_ << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const AnyStatement& statement) noexcept {
  statement.Print(output);
  return output;
}

void DeclareScalar::Print(std::ostream& output) const noexcept {
  output << "DeclareScalar(" << std::quoted(name_) << ")";
}

void DeclareArray::Print(std::ostream& output) const noexcept {
  output << "DeclareArray(" << std::quoted(name_) << ", " << size_ << ")";
}

void Assign::Print(std::ostream& output) const noexcept {
  output << "Assign(" << left_ << ", " << right_ << ")";
}

void If::Print(std::ostream& output) const noexcept {
  output << "If(" << condition_ << ", " << List(then_branch_) << ", "
         << List(else_branch_) << ")";
}

void While::Print(std::ostream& output) const noexcept {
  output << "While(" << condition_ << ", " << List(body_) << ")";
}

void Return::Print(std::ostream& output) const noexcept {
  if (value_) {
    output << "Return(" << *value_ << ")";
  } else {
    output << "Return()";
  }
}

void Break::Print(std::ostream& output) const noexcept {
  output << "Break()";
}

void Continue::Print(std::ostream& output) const noexcept {
  output << "Continue()";
}

void DiscardedExpression::Print(std::ostream& output) const noexcept {
  output << "DiscardedExpression(" << expression_ << ")";
}

}  // namespace aoc2021
