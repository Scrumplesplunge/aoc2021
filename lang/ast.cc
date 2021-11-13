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

class StatementPrinter : public StatementVisitor<void> {
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

const Location& AnyExpression::location() const {
  return std::visit([](const auto& x) -> const Location& { return x.location; },
                    value_->value);
}

const ExpressionVariant& AnyExpression::operator*() const noexcept {
  return *value_;
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

std::ostream& operator<<(std::ostream& output,
                         const AnyExpression& expression) noexcept {
  return std::visit([&](const auto& x) -> std::ostream& { return output << x; },
                    expression->value);
}

std::ostream& operator<<(std::ostream& output,
                         const AnyStatement& statement) noexcept {
  StatementPrinter printer(output);
  statement.Visit(printer);
  return output;
}

}  // namespace aoc2021::ast
