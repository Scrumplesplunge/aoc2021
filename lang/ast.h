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

template <typename T>
concept Expression = Located<T> && ValueCanHold<ExpressionVariant, T>;

class AnyExpression {
 public:
  template <Expression T>
  AnyExpression(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const Location& location() const;
  const ExpressionVariant& operator*() const noexcept;
  const ExpressionVariant* operator->() const noexcept { return &**this; }

 private:
  std::shared_ptr<const ExpressionVariant> value_;
};

struct Name {
  Location location;
  std::string value;
};

struct IntegerLiteral {
  Location location;
  std::int64_t value;
};

struct Call {
  Location location;
  AnyExpression function;
  std::vector<AnyExpression> arguments;
};

struct Index {
  Location location;
  AnyExpression container, index;
};

struct Negate {
  Location location;
  AnyExpression inner;
};

struct LogicalNot {
  Location location;
  AnyExpression inner;
};

struct BitwiseNot {
  Location location;
  AnyExpression inner;
};

struct Dereference {
  Location location;
  AnyExpression inner;
};

struct Add {
  Location location;
  AnyExpression left, right;
};

struct Subtract {
  Location location;
  AnyExpression left, right;
};

struct Multiply {
  Location location;
  AnyExpression left, right;
};

struct Divide {
  Location location;
  AnyExpression left, right;
};

struct Modulo {
  Location location;
  AnyExpression left, right;
};

struct LessThan {
  Location location;
  AnyExpression left, right;
};

struct LessOrEqual {
  Location location;
  AnyExpression left, right;
};

struct GreaterThan {
  Location location;
  AnyExpression left, right;
};

struct GreaterOrEqual {
  Location location;
  AnyExpression left, right;
};

struct Equal {
  Location location;
  AnyExpression left, right;
};

struct NotEqual {
  Location location;
  AnyExpression left, right;
};

struct LogicalAnd {
  Location location;
  AnyExpression left, right;
};

struct LogicalOr {
  Location location;
  AnyExpression left, right;
};

struct BitwiseAnd {
  Location location;
  AnyExpression left, right;
};

struct BitwiseOr {
  Location location;
  AnyExpression left, right;
};

struct BitwiseXor {
  Location location;
  AnyExpression left, right;
};

struct ShiftLeft {
  Location location;
  AnyExpression left, right;
};

struct ShiftRight {
  Location location;
  AnyExpression left, right;
};

struct TernaryExpression {
  Location location;
  AnyExpression condition, then_branch, else_branch;
};

struct ExpressionVariant {
  auto operator<=>(const ExpressionVariant&) const = default;

  std::variant<Name, IntegerLiteral, Call, Index, Negate, LogicalNot,
               BitwiseNot, Dereference, Add, Subtract, Multiply, Divide, Modulo,
               LessThan, LessOrEqual, GreaterThan, GreaterOrEqual, Equal,
               NotEqual, LogicalAnd, LogicalOr, BitwiseAnd, BitwiseOr,
               BitwiseXor, ShiftLeft, ShiftRight, TernaryExpression>
      value;
};

static_assert(ValueCanHold<ExpressionVariant, IntegerLiteral>);

template <Expression T>
AnyExpression::AnyExpression(T value) noexcept
    : value_(std::make_shared<ExpressionVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const Name&) noexcept;
std::ostream& operator<<(std::ostream&, const IntegerLiteral&) noexcept;
std::ostream& operator<<(std::ostream&, const Call&) noexcept;
std::ostream& operator<<(std::ostream&, const Index&) noexcept;
std::ostream& operator<<(std::ostream&, const Negate&) noexcept;
std::ostream& operator<<(std::ostream&, const LogicalNot&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseNot&) noexcept;
std::ostream& operator<<(std::ostream&, const Dereference&) noexcept;
std::ostream& operator<<(std::ostream&, const Add&) noexcept;
std::ostream& operator<<(std::ostream&, const Subtract&) noexcept;
std::ostream& operator<<(std::ostream&, const Multiply&) noexcept;
std::ostream& operator<<(std::ostream&, const Divide&) noexcept;
std::ostream& operator<<(std::ostream&, const Modulo&) noexcept;
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
std::ostream& operator<<(std::ostream&, const AnyExpression&) noexcept;

struct StatementVariant;

template <typename T>
concept Statement = Located<T> && ValueCanHold<StatementVariant, T>;

class AnyStatement {
 public:
  // Implicit conversion from any type of statement.
  template <Statement T>
  AnyStatement(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const Location& location() const noexcept;
  const StatementVariant& operator*() const noexcept;
  const StatementVariant* operator->() const noexcept { return &**this; }

 private:
  std::shared_ptr<const StatementVariant> value_;
};

struct DeclareScalar {
  Location location;
  std::string name;
};

struct DeclareArray {
  Location location;
  std::string name;
  AnyExpression size;
};

struct Assign {
  Location location;
  AnyExpression left, right;
};

struct If {
  Location location;
  AnyExpression condition;
  std::vector<AnyStatement> then_branch, else_branch;
};

struct While {
  Location location;
  AnyExpression condition;
  std::vector<AnyStatement> body;
};

struct Return {
  Location location;
  std::optional<AnyExpression> value = std::nullopt;
};

struct Break {
  Location location;
};

struct Continue {
  Location location;
};

struct DiscardedExpression {
  AnyExpression expression;
  Location location = expression.location();
};

struct FunctionDefinition {
  Location location;
  std::string name;
  std::vector<Name> parameters;
  std::vector<AnyStatement> body;
};

struct StatementVariant {
  auto operator<=>(const StatementVariant&) const = default;

  std::variant<DeclareScalar, DeclareArray, Assign, If, While, Return, Break,
               Continue, DiscardedExpression, FunctionDefinition>
      value;
};

template <Statement T>
AnyStatement::AnyStatement(T value) noexcept
    : value_(std::make_shared<StatementVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const DeclareScalar&) noexcept;
std::ostream& operator<<(std::ostream&, const DeclareArray&) noexcept;
std::ostream& operator<<(std::ostream&, const Assign&) noexcept;
std::ostream& operator<<(std::ostream&, const If&) noexcept;
std::ostream& operator<<(std::ostream&, const While&) noexcept;
std::ostream& operator<<(std::ostream&, const Return&) noexcept;
std::ostream& operator<<(std::ostream&, const Break&) noexcept;
std::ostream& operator<<(std::ostream&, const Continue&) noexcept;
std::ostream& operator<<(std::ostream&, const DiscardedExpression&) noexcept;
std::ostream& operator<<(std::ostream&, const FunctionDefinition&) noexcept;
std::ostream& operator<<(std::ostream&, const AnyStatement&) noexcept;

}  // namespace aoc2021::ast

#endif  // AST_H_
