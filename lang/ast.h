#ifndef AST_H_
#define AST_H_

#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "source.h"
#include "variant_utils.h"

namespace aoc2021::ast {

template <typename T>
concept Located = requires (const T& t) {
  { t.location } -> std::same_as<const Location&>;
};

struct ExpressionVariant;

class Expression {
 public:
  template <HoldableBy<ExpressionVariant> T>
  Expression(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const Location& location() const;
  const ExpressionVariant& operator*() const noexcept;
  const ExpressionVariant* operator->() const noexcept { return &**this; }

  bool operator==(const Expression&) const;
  std::strong_ordering operator<=>(const Expression&) const;

 private:
  std::shared_ptr<const ExpressionVariant> value_;
};

struct Name {
  bool operator==(const Name&) const = default;
  auto operator<=>(const Name&) const = default;

  Location location;
  std::string value;
};

struct CharacterLiteral {
  bool operator==(const CharacterLiteral&) const = default;
  auto operator<=>(const CharacterLiteral&) const = default;

  Location location;
  char value;
};

struct IntegerLiteral {
  bool operator==(const IntegerLiteral&) const = default;
  auto operator<=>(const IntegerLiteral&) const = default;

  Location location;
  std::int64_t value;
};

struct StringLiteral {
  bool operator==(const StringLiteral&) const = default;
  auto operator<=>(const StringLiteral&) const = default;

  Location location;
  std::string value;
};

struct Access {
  bool operator==(const Access&) const = default;
  auto operator<=>(const Access&) const = default;

  Location location;
  Expression object;
  Name field;
};

struct Call {
  bool operator==(const Call&) const = default;
  auto operator<=>(const Call&) const = default;

  Location location;
  Expression function;
  std::vector<Expression> arguments;
};

struct Index {
  bool operator==(const Index&) const = default;
  auto operator<=>(const Index&) const = default;

  Location location;
  Expression container, index;
};

struct Negate {
  bool operator==(const Negate&) const = default;
  auto operator<=>(const Negate&) const = default;

  Location location;
  Expression inner;
};

struct LogicalNot {
  bool operator==(const LogicalNot&) const = default;
  auto operator<=>(const LogicalNot&) const = default;

  Location location;
  Expression inner;
};

struct BitwiseNot {
  bool operator==(const BitwiseNot&) const = default;
  auto operator<=>(const BitwiseNot&) const = default;

  Location location;
  Expression inner;
};

struct Dereference {
  bool operator==(const Dereference&) const = default;
  auto operator<=>(const Dereference&) const = default;

  Location location;
  Expression inner;
};

struct AddressOf {
  bool operator==(const AddressOf&) const = default;
  auto operator<=>(const AddressOf&) const = default;

  Location location;
  Expression inner;
};

struct Add {
  bool operator==(const Add&) const = default;
  auto operator<=>(const Add&) const = default;

  Location location;
  Expression left, right;
};

struct Subtract {
  bool operator==(const Subtract&) const = default;
  auto operator<=>(const Subtract&) const = default;

  Location location;
  Expression left, right;
};

struct Multiply {
  bool operator==(const Multiply&) const = default;
  auto operator<=>(const Multiply&) const = default;

  Location location;
  Expression left, right;
};

struct Divide {
  bool operator==(const Divide&) const = default;
  auto operator<=>(const Divide&) const = default;

  Location location;
  Expression left, right;
};

struct Modulo {
  bool operator==(const Modulo&) const = default;
  auto operator<=>(const Modulo&) const = default;

  Location location;
  Expression left, right;
};

struct As {
  bool operator==(const As&) const = default;
  auto operator<=>(const As&) const = default;

  Location location;
  Expression value, type;
};

struct LessThan {
  bool operator==(const LessThan&) const = default;
  auto operator<=>(const LessThan&) const = default;

  Location location;
  Expression left, right;
};

struct LessOrEqual {
  bool operator==(const LessOrEqual&) const = default;
  auto operator<=>(const LessOrEqual&) const = default;

  Location location;
  Expression left, right;
};

struct GreaterThan {
  bool operator==(const GreaterThan&) const = default;
  auto operator<=>(const GreaterThan&) const = default;

  Location location;
  Expression left, right;
};

struct GreaterOrEqual {
  bool operator==(const GreaterOrEqual&) const = default;
  auto operator<=>(const GreaterOrEqual&) const = default;

  Location location;
  Expression left, right;
};

struct Equal {
  bool operator==(const Equal&) const = default;
  auto operator<=>(const Equal&) const = default;

  Location location;
  Expression left, right;
};

struct NotEqual {
  bool operator==(const NotEqual&) const = default;
  auto operator<=>(const NotEqual&) const = default;

  Location location;
  Expression left, right;
};

struct LogicalAnd {
  bool operator==(const LogicalAnd&) const = default;
  auto operator<=>(const LogicalAnd&) const = default;

  Location location;
  Expression left, right;
};

struct LogicalOr {
  bool operator==(const LogicalOr&) const = default;
  auto operator<=>(const LogicalOr&) const = default;

  Location location;
  Expression left, right;
};

struct BitwiseAnd {
  bool operator==(const BitwiseAnd&) const = default;
  auto operator<=>(const BitwiseAnd&) const = default;

  Location location;
  Expression left, right;
};

struct BitwiseOr {
  bool operator==(const BitwiseOr&) const = default;
  auto operator<=>(const BitwiseOr&) const = default;

  Location location;
  Expression left, right;
};

struct BitwiseXor {
  bool operator==(const BitwiseXor&) const = default;
  auto operator<=>(const BitwiseXor&) const = default;

  Location location;
  Expression left, right;
};

struct ShiftLeft {
  bool operator==(const ShiftLeft&) const = default;
  auto operator<=>(const ShiftLeft&) const = default;

  Location location;
  Expression left, right;
};

struct ShiftRight {
  bool operator==(const ShiftRight&) const = default;
  auto operator<=>(const ShiftRight&) const = default;

  Location location;
  Expression left, right;
};

struct TernaryExpression {
  bool operator==(const TernaryExpression&) const = default;
  auto operator<=>(const TernaryExpression&) const = default;

  Location location;
  Expression condition, then_branch, else_branch;
};

struct ArrayType {
  bool operator==(const ArrayType&) const = default;
  auto operator<=>(const ArrayType&) const = default;

  Location location;
  Expression size;
  Expression element_type;
};

struct SpanType {
  bool operator==(const SpanType&) const = default;
  auto operator<=>(const SpanType&) const = default;

  Location location;
  Expression element_type;
};

struct ExpressionVariant {
  bool operator==(const ExpressionVariant&) const = default;
  auto operator<=>(const ExpressionVariant&) const = default;

  std::variant<Name, CharacterLiteral, IntegerLiteral, StringLiteral, Access,
               Call, Index, Negate, LogicalNot, BitwiseNot, Dereference,
               AddressOf, Add, Subtract, Multiply, Divide, Modulo, As, LessThan,
               LessOrEqual, GreaterThan, GreaterOrEqual, Equal, NotEqual,
               LogicalAnd, LogicalOr, BitwiseAnd, BitwiseOr, BitwiseXor,
               ShiftLeft, ShiftRight, TernaryExpression, ArrayType, SpanType>
      value;
};

template <HoldableBy<ExpressionVariant> T>
Expression::Expression(T value) noexcept
    : value_(std::make_shared<ExpressionVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const Name&) noexcept;
std::ostream& operator<<(std::ostream&, const CharacterLiteral&) noexcept;
std::ostream& operator<<(std::ostream&, const IntegerLiteral&) noexcept;
std::ostream& operator<<(std::ostream&, const StringLiteral&) noexcept;
std::ostream& operator<<(std::ostream&, const Access&) noexcept;
std::ostream& operator<<(std::ostream&, const Call&) noexcept;
std::ostream& operator<<(std::ostream&, const Index&) noexcept;
std::ostream& operator<<(std::ostream&, const Negate&) noexcept;
std::ostream& operator<<(std::ostream&, const LogicalNot&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseNot&) noexcept;
std::ostream& operator<<(std::ostream&, const Dereference&) noexcept;
std::ostream& operator<<(std::ostream&, const AddressOf&) noexcept;
std::ostream& operator<<(std::ostream&, const Add&) noexcept;
std::ostream& operator<<(std::ostream&, const Subtract&) noexcept;
std::ostream& operator<<(std::ostream&, const Multiply&) noexcept;
std::ostream& operator<<(std::ostream&, const Divide&) noexcept;
std::ostream& operator<<(std::ostream&, const Modulo&) noexcept;
std::ostream& operator<<(std::ostream&, const As&) noexcept;
std::ostream& operator<<(std::ostream&, const LessThan&) noexcept;
std::ostream& operator<<(std::ostream&, const LessOrEqual&) noexcept;
std::ostream& operator<<(std::ostream&, const GreaterThan&) noexcept;
std::ostream& operator<<(std::ostream&, const GreaterOrEqual&) noexcept;
std::ostream& operator<<(std::ostream&, const Equal&) noexcept;
std::ostream& operator<<(std::ostream&, const NotEqual&) noexcept;
std::ostream& operator<<(std::ostream&, const LogicalAnd&) noexcept;
std::ostream& operator<<(std::ostream&, const LogicalOr&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseAnd&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseOr&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseXor&) noexcept;
std::ostream& operator<<(std::ostream&, const ShiftLeft&) noexcept;
std::ostream& operator<<(std::ostream&, const ShiftRight&) noexcept;
std::ostream& operator<<(std::ostream&, const TernaryExpression&) noexcept;
std::ostream& operator<<(std::ostream&, const ArrayType&) noexcept;
std::ostream& operator<<(std::ostream&, const SpanType&) noexcept;
std::ostream& operator<<(std::ostream&, const Expression&) noexcept;

struct StatementVariant;

class Statement {
 public:
  // Implicit conversion from any type of statement.
  template <HoldableBy<StatementVariant> T>
  Statement(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const Location& location() const noexcept;
  const StatementVariant& operator*() const noexcept;
  const StatementVariant* operator->() const noexcept { return &**this; }

  bool operator==(const Statement&) const;
  std::strong_ordering operator<=>(const Statement&) const;

 private:
  std::shared_ptr<const StatementVariant> value_;
};

struct Import {
  bool operator==(const Import&) const = default;
  auto operator<=>(const Import&) const = default;

  Location location;
  std::string path;
  Name alias;
};

struct Export {
  bool operator==(const Export&) const = default;
  auto operator<=>(const Export&) const = default;

  Location location;
  Statement target;
};

struct DeclareVariable {
  bool operator==(const DeclareVariable&) const = default;
  auto operator<=>(const DeclareVariable&) const = default;

  Location location;
  std::string name;
  Expression type;
};

struct Assign {
  bool operator==(const Assign&) const = default;
  auto operator<=>(const Assign&) const = default;

  Location location;
  Expression left, right;
};

struct DeclareAndAssign {
  bool operator==(const DeclareAndAssign&) const = default;
  auto operator<=>(const DeclareAndAssign&) const = default;

  Location location;
  std::string name;
  Expression type;
  Expression value;
};

struct If {
  bool operator==(const If&) const = default;
  auto operator<=>(const If&) const = default;

  Location location;
  Expression condition;
  std::vector<Statement> then_branch, else_branch;
};

struct While {
  bool operator==(const While&) const = default;
  auto operator<=>(const While&) const = default;

  Location location;
  Expression condition;
  std::vector<Statement> body;
};

struct Return {
  bool operator==(const Return&) const = default;
  auto operator<=>(const Return&) const = default;

  Location location;
  std::optional<Expression> value = std::nullopt;
};

struct Break {
  bool operator==(const Break&) const = default;
  auto operator<=>(const Break&) const = default;

  Location location;
};

struct Continue {
  bool operator==(const Continue&) const = default;
  auto operator<=>(const Continue&) const = default;

  Location location;
};

struct DiscardedExpression {
  bool operator==(const DiscardedExpression&) const = default;
  auto operator<=>(const DiscardedExpression&) const = default;

  Expression expression;
  Location location = expression.location();
};

struct FunctionDefinition {
  struct Parameter {
    bool operator==(const Parameter&) const = default;
    auto operator<=>(const Parameter&) const = default;

    Name name;
    Expression type;
  };

  bool operator==(const FunctionDefinition&) const = default;
  auto operator<=>(const FunctionDefinition&) const = default;

  Location location;
  std::string name;
  std::vector<Parameter> parameters;
  Expression return_type;
  std::vector<Statement> body;
};

struct StatementVariant {
  bool operator==(const StatementVariant&) const = default;
  auto operator<=>(const StatementVariant&) const = default;

  std::variant<Import, Export, DeclareVariable, Assign, DeclareAndAssign, If,
               While, Return, Break, Continue, DiscardedExpression,
               FunctionDefinition>
      value;
};

template <HoldableBy<StatementVariant> T>
Statement::Statement(T value) noexcept
    : value_(std::make_shared<StatementVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const Import&) noexcept;
std::ostream& operator<<(std::ostream&, const Export&) noexcept;
std::ostream& operator<<(std::ostream&, const DeclareVariable&) noexcept;
std::ostream& operator<<(std::ostream&, const Assign&) noexcept;
std::ostream& operator<<(std::ostream&, const DeclareAndAssign&) noexcept;
std::ostream& operator<<(std::ostream&, const If&) noexcept;
std::ostream& operator<<(std::ostream&, const While&) noexcept;
std::ostream& operator<<(std::ostream&, const Return&) noexcept;
std::ostream& operator<<(std::ostream&, const Break&) noexcept;
std::ostream& operator<<(std::ostream&, const Continue&) noexcept;
std::ostream& operator<<(std::ostream&, const DiscardedExpression&) noexcept;
std::ostream& operator<<(std::ostream&,
                         const FunctionDefinition::Parameter&) noexcept;
std::ostream& operator<<(std::ostream&, const FunctionDefinition&) noexcept;
std::ostream& operator<<(std::ostream&, const Statement&) noexcept;

}  // namespace aoc2021::ast

#endif  // AST_H_
