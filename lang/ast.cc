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

class ExpressionPrinter : public ExpressionVisitor<void> {
 public:
  ExpressionPrinter(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const Name& x) override {
    *output_ << "Name(" << std::quoted(x.value) << ")";
  }

  void operator()(const IntegerLiteral& x) override {
    *output_ << "IntegerLiteral(" << x.value << ")";
  }

  void operator()(const Call& x) override {
    *output_ << "Call(" << x.function << ", " << List(x.arguments) << ")";
  }

  void operator()(const Index& x) override {
    *output_ << "Index(" << x.container << ", " << x.index << ")";
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

  void operator()(const Dereference& x) override {
    *output_ << "Dereference(" << x.inner << ")";
  }

  void operator()(const Add& x) override {
    *output_ << "Add(" << x.left << ", " << x.right << ")";
  }

  void operator()(const Subtract& x) override {
    *output_ << "Subtract(" << x.left << ", " << x.right << ")";
  }

  void operator()(const Multiply& x) override {
    *output_ << "Multiply(" << x.left << ", " << x.right << ")";
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

  void operator()(const GreaterThan& x) override {
    *output_ << "GreaterThan(" << x.left << ", " << x.right << ")";
  }

  void operator()(const GreaterOrEqual& x) override {
    *output_ << "GreaterOrEqual(" << x.left << ", " << x.right << ")";
  }

  void operator()(const Equal& x) override {
    *output_ << "Equal(" << x.left << ", " << x.right << ")";
  }

  void operator()(const NotEqual& x) override {
    *output_ << "NotEqual(" << x.left << ", " << x.right << ")";
  }

  void operator()(const LogicalAnd& x) override {
    *output_ << "LogicalAnd(" << x.left << ", " << x.right << ")";
  }

  void operator()(const LogicalOr& x) override {
    *output_ << "LogicalOr(" << x.left << ", " << x.right << ")";
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

  void operator()(const TernaryExpression& x) override {
    *output_ << "TernaryExpression(" << x.condition << ", " << x.then_branch
             << ", " << x.else_branch << ")";
  }

 private:
  std::ostream* output_;
};

class StatementPrinter : public StatementVisitor {
 public:
  StatementPrinter(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const DeclareScalar& x) override {
    *output_ << "DeclareScalar(" << std::quoted(x.name) << ")";
  }

  void operator()(const DeclareArray& x) override {
    *output_ << "DeclareArray(" << std::quoted(x.name) << ", " << x.size << ")";
  }

  void operator()(const Assign& x) override {
    *output_ << "Assign(" << x.left << ", " << x.right << ")";
  }

  void operator()(const If& x) override {
    *output_ << "If(" << x.condition << ", " << List(x.then_branch) << ", "
             << List(x.else_branch) << ")";
  }

  void operator()(const While& x) override {
    *output_ << "While(" << x.condition << ", " << List(x.body) << ")";
  }

  void operator()(const Return& x) override {
    if (x.value) {
      *output_ << "Return(" << *x.value << ")";
    } else {
      *output_ << "Return()";
    }
  }

  void operator()(const Break&) override {
    *output_ << "Break()";
  }

  void operator()(const Continue&) override {
    *output_ << "Continue()";
  }

  void operator()(const DiscardedExpression& x) override {
    *output_ << "DiscardedExpression(" << x.expression << ")";
  }

  void operator()(const FunctionDefinition& x) override {
    *output_ << "FunctionDefinition(" << std::quoted(x.name) << ", "
             << List(x.parameters) << ", " << List(x.body) << ")";
  }

 private:
  std::ostream* output_;
};

}  // namespace

std::ostream& operator<<(std::ostream& output,
                         const AnyExpression& expression) noexcept {
  ExpressionPrinter printer(output);
  expression.Visit(printer);
  return output;
}

std::ostream& operator<<(std::ostream& output,
                         const AnyStatement& statement) noexcept {
  StatementPrinter printer(output);
  statement.Visit(printer);
  return output;
}

}  // namespace aoc2021::ast
