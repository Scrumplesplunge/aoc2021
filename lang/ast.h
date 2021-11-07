#ifndef AST_H_
#define AST_H_

#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "source.h"

namespace aoc2021::ast {

template <typename T>
concept Located = requires (const T& t) {
  { t.location } -> std::same_as<const Location&>;
};

template <typename T>
concept Printable = requires (const T& value, std::ostream& output) {
  Print(value, output);
};

template <typename T>
struct ExpressionVisitor;

template <typename T>
concept Expression =
    Located<T> && std::invocable<ExpressionVisitor<void>&, const T&>;

class AnyExpression {
 public:
  // Implicit conversion from any type of expression.
  template <Expression T>
  AnyExpression(T value) noexcept : value_(new Adaptor<T>(std::move(value))) {}

  const Location& location() const noexcept { return value_->location(); }

  void Visit(ExpressionVisitor<void>& visitor) const {
    return value_->Visit(visitor);
  }

  template <typename T> T Visit(ExpressionVisitor<T>& visitor) const;

  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  struct Interface {
    virtual ~Interface() = default;
    virtual const Location& location() const noexcept = 0;
    virtual void Visit(ExpressionVisitor<void>&) const = 0;
  };

  template <Expression T>
  class Adaptor : public Interface {
   public:
    explicit Adaptor(T value) noexcept : value_(std::move(value)) {}

    const Location& location() const noexcept override {
      return value_.location;
    }

    void Visit(ExpressionVisitor<void>& visitor) const override {
      visitor(value_);
    }

   private:
    T value_;
  };

  std::unique_ptr<Interface> value_;
};

std::ostream& operator<<(std::ostream&, const AnyExpression&) noexcept;

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

template <typename T>
struct ExpressionVisitor {
  virtual ~ExpressionVisitor() = default;
  virtual T operator()(const Name&) = 0;
  virtual T operator()(const IntegerLiteral&) = 0;
  virtual T operator()(const Call&) = 0;
  virtual T operator()(const Index&) = 0;
  virtual T operator()(const Negate&) = 0;
  virtual T operator()(const LogicalNot&) = 0;
  virtual T operator()(const BitwiseNot&) = 0;
  virtual T operator()(const Dereference&) = 0;
  virtual T operator()(const Add&) = 0;
  virtual T operator()(const Subtract&) = 0;
  virtual T operator()(const Multiply&) = 0;
  virtual T operator()(const Divide&) = 0;
  virtual T operator()(const Modulo&) = 0;
  virtual T operator()(const LessThan&) = 0;
  virtual T operator()(const LessOrEqual&) = 0;
  virtual T operator()(const GreaterThan&) = 0;
  virtual T operator()(const GreaterOrEqual&) = 0;
  virtual T operator()(const Equal&) = 0;
  virtual T operator()(const NotEqual&) = 0;
  virtual T operator()(const LogicalAnd&) = 0;
  virtual T operator()(const LogicalOr&) = 0;
  virtual T operator()(const BitwiseAnd&) = 0;
  virtual T operator()(const BitwiseOr&) = 0;
  virtual T operator()(const BitwiseXor&) = 0;
  virtual T operator()(const ShiftLeft&) = 0;
  virtual T operator()(const ShiftRight&) = 0;
  virtual T operator()(const TernaryExpression&) = 0;
};

template <typename T>
T AnyExpression::Visit(ExpressionVisitor<T>& visitor) const {
  struct ProxyVisitor : ExpressionVisitor<void> {
    ProxyVisitor(ExpressionVisitor<T>& f) noexcept : f(f) {}
    void operator()(const Name& x) override { new (result) T(f(x)); }
    void operator()(const IntegerLiteral& x) override { new (result) T(f(x)); }
    void operator()(const Call& x) override { new (result) T(f(x)); }
    void operator()(const Index& x) override { new (result) T(f(x)); }
    void operator()(const Negate& x) override { new (result) T(f(x)); }
    void operator()(const LogicalNot& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseNot& x) override { new (result) T(f(x)); }
    void operator()(const Dereference& x) override { new (result) T(f(x)); }
    void operator()(const Add& x) override { new (result) T(f(x)); }
    void operator()(const Subtract& x) override { new (result) T(f(x)); }
    void operator()(const Multiply& x) override { new (result) T(f(x)); }
    void operator()(const Divide& x) override { new (result) T(f(x)); }
    void operator()(const Modulo& x) override { new (result) T(f(x)); }
    void operator()(const LessThan& x) override { new (result) T(f(x)); }
    void operator()(const LessOrEqual& x) override { new (result) T(f(x)); }
    void operator()(const GreaterThan& x) override { new (result) T(f(x)); }
    void operator()(const GreaterOrEqual& x) override { new (result) T(f(x)); }
    void operator()(const Equal& x) override { new (result) T(f(x)); }
    void operator()(const NotEqual& x) override { new (result) T(f(x)); }
    void operator()(const LogicalAnd& x) override { new (result) T(f(x)); }
    void operator()(const LogicalOr& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseAnd& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseOr& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseXor& x) override { new (result) T(f(x)); }
    void operator()(const ShiftLeft& x) override { new (result) T(f(x)); }
    void operator()(const ShiftRight& x) override { new (result) T(f(x)); }
    void operator()(const TernaryExpression& x) override {
      new (result) T(f(x));
    }

    T Consume() && { return std::move(*(T*)result); }

    ExpressionVisitor<T>& f;
    alignas(T) char result[sizeof(T)];
  };
  ProxyVisitor v{visitor};
  Visit(v);
  return std::move(v).Consume();
}

class ExpressionPrinter : public ExpressionVisitor<void> {
 public:
  explicit ExpressionPrinter(std::ostream& output) noexcept
      : output_(&output) {}
  void operator()(const Name&) override;
  void operator()(const IntegerLiteral&) override;
  void operator()(const Call&) override;
  void operator()(const Index&) override;
  void operator()(const Negate&) override;
  void operator()(const LogicalNot&) override;
  void operator()(const BitwiseNot&) override;
  void operator()(const Dereference&) override;
  void operator()(const Add&) override;
  void operator()(const Subtract&) override;
  void operator()(const Multiply&) override;
  void operator()(const Divide&) override;
  void operator()(const Modulo&) override;
  void operator()(const LessThan&) override;
  void operator()(const LessOrEqual&) override;
  void operator()(const GreaterThan&) override;
  void operator()(const GreaterOrEqual&) override;
  void operator()(const Equal&) override;
  void operator()(const NotEqual&) override;
  void operator()(const LogicalAnd&) override;
  void operator()(const LogicalOr&) override;
  void operator()(const BitwiseAnd&) override;
  void operator()(const BitwiseOr&) override;
  void operator()(const BitwiseXor&) override;
  void operator()(const ShiftLeft&) override;
  void operator()(const ShiftRight&) override;
  void operator()(const TernaryExpression&) override;

 private:
  std::ostream* output_;
};

struct StatementVisitor;

template <typename T>
concept Statement = Located<T> && std::invocable<StatementVisitor&, const T&>;

class AnyStatement {
 public:
  // Implicit conversion from any type of statement.
  template <Statement T>
  AnyStatement(T value) noexcept : value_(new Adaptor<T>(std::move(value))) {}

  const Location& location() const noexcept { return value_->location(); }
  void Visit(StatementVisitor& visitor) const { return value_->Visit(visitor); }
  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  struct Interface {
    virtual ~Interface() = default;
    virtual const Location& location() const noexcept = 0;
    virtual void Visit(StatementVisitor&) const = 0;
  };

  template <Statement T>
  class Adaptor : public Interface {
   public:
    explicit Adaptor(T value) noexcept : value_(std::move(value)) {}

    const Location& location() const noexcept override {
      return value_.location;
    }

    void Visit(StatementVisitor& visitor) const override { visitor(value_); }

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

struct StatementVisitor {
  virtual ~StatementVisitor() = default;
  virtual void operator()(const DeclareScalar&) = 0;
  virtual void operator()(const DeclareArray&) = 0;
  virtual void operator()(const Assign&) = 0;
  virtual void operator()(const If&) = 0;
  virtual void operator()(const While&) = 0;
  virtual void operator()(const Return&) = 0;
  virtual void operator()(const Break&) = 0;
  virtual void operator()(const Continue&) = 0;
  virtual void operator()(const DiscardedExpression&) = 0;
  virtual void operator()(const FunctionDefinition&) = 0;
};

class StatementPrinter : public StatementVisitor {
 public:
  explicit StatementPrinter(std::ostream& output) noexcept : output_(&output) {}
  virtual void operator()(const DeclareScalar&) override;
  virtual void operator()(const DeclareArray&) override;
  virtual void operator()(const Assign&) override;
  virtual void operator()(const If&) override;
  virtual void operator()(const While&) override;
  virtual void operator()(const Return&) override;
  virtual void operator()(const Break&) override;
  virtual void operator()(const Continue&) override;
  virtual void operator()(const DiscardedExpression&) override;
  virtual void operator()(const FunctionDefinition&) override;

 private:
  std::ostream* output_;
};

static_assert(Statement<Continue>);

}  // namespace aoc2021::ast

#endif  // AST_H_
