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

namespace aoc2021::ast {

template <typename T>
concept Located = requires (const T& t) {
  { t.location } -> std::same_as<const Location&>;
};

struct ExpressionVariant;

template <typename T>
concept Expression =
    Located<T> && std::constructible_from<ExpressionVariant, T>;

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

template <typename T>
struct StatementVisitor;

template <typename T>
concept Statement =
    Located<T> && std::invocable<StatementVisitor<void>&, const T&>;

class AnyStatement {
 public:
  // Implicit conversion from any type of statement.
  template <Statement T>
  AnyStatement(T value) noexcept : value_(new Adaptor<T>(std::move(value))) {}

  const Location& location() const noexcept { return value_->location(); }

  void Visit(StatementVisitor<void>& visitor) const {
    return value_->Visit(visitor);
  }

  template <typename T>
  T Visit(StatementVisitor<T>& visitor) const;

  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  struct Interface {
    virtual ~Interface() = default;
    virtual const Location& location() const noexcept = 0;
    virtual void Visit(StatementVisitor<void>&) const = 0;
  };

  template <Statement T>
  class Adaptor : public Interface {
   public:
    explicit Adaptor(T value) noexcept : value_(std::move(value)) {}

    const Location& location() const noexcept override {
      return value_.location;
    }

    void Visit(StatementVisitor<void>& visitor) const override {
      visitor(value_);
    }

   private:
    T value_;
  };

  std::unique_ptr<Interface> value_;
};

std::ostream& operator<<(std::ostream&, const AnyStatement&) noexcept;

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

template <typename T>
struct StatementVisitor {
  virtual ~StatementVisitor() = default;
  virtual T operator()(const DeclareScalar&) = 0;
  virtual T operator()(const DeclareArray&) = 0;
  virtual T operator()(const Assign&) = 0;
  virtual T operator()(const If&) = 0;
  virtual T operator()(const While&) = 0;
  virtual T operator()(const Return&) = 0;
  virtual T operator()(const Break&) = 0;
  virtual T operator()(const Continue&) = 0;
  virtual T operator()(const DiscardedExpression&) = 0;
  virtual T operator()(const FunctionDefinition&) = 0;
};

template <typename T>
T AnyStatement::Visit(StatementVisitor<T>& visitor) const {
  struct ProxyVisitor : StatementVisitor<void> {
    ProxyVisitor(StatementVisitor<T>& f) noexcept : f(f) {}
    void operator()(const DeclareScalar& x) override { new (result) T(f(x)); }
    void operator()(const DeclareArray& x) override { new (result) T(f(x)); }
    void operator()(const Assign& x) override { new (result) T(f(x)); }
    void operator()(const If& x) override { new (result) T(f(x)); }
    void operator()(const While& x) override { new (result) T(f(x)); }
    void operator()(const Return& x) override { new (result) T(f(x)); }
    void operator()(const Break& x) override { new (result) T(f(x)); }
    void operator()(const Continue& x) override { new (result) T(f(x)); }
    void operator()(const DiscardedExpression& x) override {
      new (result) T(f(x));
    }
    void operator()(const FunctionDefinition& x) override {
      new (result) T(f(x));
    }

    T Consume() && { return std::move(*(T*)result); }

    StatementVisitor<T>& f;
    alignas(T) char result[sizeof(T)];
  };
  ProxyVisitor v{visitor};
  Visit(v);
  return std::move(v).Consume();
}

}  // namespace aoc2021::ast

#endif  // AST_H_
