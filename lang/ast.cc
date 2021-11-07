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

std::ostream& operator<<(std::ostream& output,
                         const AnyExpression& expression) noexcept {
  ExpressionPrinter printer(output);
  expression.Visit(printer);
  return output;
}

void ExpressionPrinter::operator()(const Name& x) {
  *output_ << "Name(" << std::quoted(x.value) << ")";
}

void ExpressionPrinter::operator()(const IntegerLiteral& x) {
  *output_ << "IntegerLiteral(" << x.value << ")";
}

void ExpressionPrinter::operator()(const Call& x) {
  *output_ << "Call(" << x.function << ", " << List(x.arguments) << ")";
}

void ExpressionPrinter::operator()(const Index& x) {
  *output_ << "Index(" << x.container << ", " << x.index << ")";
}

void ExpressionPrinter::operator()(const Negate& x) {
  *output_ << "Negate(" << x.inner << ")";
}

void ExpressionPrinter::operator()(const LogicalNot& x) {
  *output_ << "LogicalNot(" << x.inner << ")";
}

void ExpressionPrinter::operator()(const BitwiseNot& x) {
  *output_ << "BitwiseNot(" << x.inner << ")";
}

void ExpressionPrinter::operator()(const Dereference& x) {
  *output_ << "Dereference(" << x.inner << ")";
}

void ExpressionPrinter::operator()(const Add& x) {
  *output_ << "Add(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const Subtract& x) {
  *output_ << "Subtract(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const Multiply& x) {
  *output_ << "Multiply(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const Divide& x) {
  *output_ << "Divide(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const Modulo& x) {
  *output_ << "Modulo(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const LessThan& x) {
  *output_ << "LessThan(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const LessOrEqual& x) {
  *output_ << "LessOrEqual(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const GreaterThan& x) {
  *output_ << "GreaterThan(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const GreaterOrEqual& x) {
  *output_ << "GreaterOrEqual(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const Equal& x) {
  *output_ << "Equal(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const NotEqual& x) {
  *output_ << "NotEqual(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const LogicalAnd& x) {
  *output_ << "LogicalAnd(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const LogicalOr& x) {
  *output_ << "LogicalOr(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const BitwiseAnd& x) {
  *output_ << "BitwiseAnd(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const BitwiseOr& x) {
  *output_ << "BitwiseOr(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const BitwiseXor& x) {
  *output_ << "BitwiseXor(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const ShiftLeft& x) {
  *output_ << "ShiftLeft(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const ShiftRight& x) {
  *output_ << "ShiftRight(" << x.left << ", " << x.right << ")";
}

void ExpressionPrinter::operator()(const TernaryExpression& x) {
  *output_ << "TernaryExpression(" << x.condition << ", " << x.then_branch
           << ", " << x.else_branch << ")";
}

std::ostream& operator<<(std::ostream& output,
                         const AnyStatement& statement) noexcept {
  StatementPrinter printer(output);
  statement.Visit(printer);
  return output;
}

void StatementPrinter::operator()(const DeclareScalar& x) {
  *output_ << "DeclareScalar(" << std::quoted(x.name) << ")";
}

void StatementPrinter::operator()(const DeclareArray& x) {
  *output_ << "DeclareArray(" << std::quoted(x.name) << ", " << x.size << ")";
}

void StatementPrinter::operator()(const Assign& x) {
  *output_ << "Assign(" << x.left << ", " << x.right << ")";
}

void StatementPrinter::operator()(const If& x) {
  *output_ << "If(" << x.condition << ", " << List(x.then_branch) << ", "
           << List(x.else_branch) << ")";
}

void StatementPrinter::operator()(const While& x) {
  *output_ << "While(" << x.condition << ", " << List(x.body) << ")";
}

void StatementPrinter::operator()(const Return& x) {
  if (x.value) {
    *output_ << "Return(" << *x.value << ")";
  } else {
    *output_ << "Return()";
  }
}

void StatementPrinter::operator()(const Break&) { *output_ << "Break()"; }
void StatementPrinter::operator()(const Continue&) { *output_ << "Continue()"; }

void StatementPrinter::operator()(const DiscardedExpression& x) {
  *output_ << "DiscardedExpression(" << x.expression << ")";
}

void StatementPrinter::operator()(const FunctionDefinition& x) {
  *output_ << "FunctionDefinition(" << std::quoted(x.name) << ", "
           << List(x.arguments) << ", " << List(x.body) << ")";
}

}  // namespace aoc2021::ast
