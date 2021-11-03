#ifndef AST_H_
#define AST_H_

#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "source.h"

namespace aoc2021 {

class Expression {
 public:
  Expression(Location location) noexcept : location_(location) {}

  virtual ~Expression() = default;
  virtual void Print(std::ostream& output) const noexcept = 0;
  const Location& location() const noexcept { return location_; }

 private:
  Location location_;
};

class AnyExpression {
 public:
  // Implicit conversion from any type of expression.
  template <typename T>
  AnyExpression(T&& value)
      : value_(new std::decay_t<T>(std::forward<T>(value))) {}
  void Print(std::ostream& output) const noexcept { value_->Print(output); }
  const Location& location() const noexcept { return value_->location(); }

  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  std::unique_ptr<Expression> value_;
};

std::ostream& operator<<(std::ostream&, const AnyExpression&) noexcept;

class Name : public Expression {
 public:
  Name(Location location, std::string_view value) noexcept
      : Expression(location), value_(value) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  std::string value_;
};

class IntegerLiteral : public Expression {
 public:
  IntegerLiteral(Location location, std::int64_t value) noexcept
      : Expression(location), value_(value) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  std::int64_t value_;
};

class Call : public Expression {
 public:
  Call(Location location, AnyExpression function,
       std::vector<AnyExpression> arguments) noexcept
      : Expression(location),
        function_(std::move(function)),
        arguments_(std::move(arguments)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression function_;
  std::vector<AnyExpression> arguments_;
};

class Negate : public Expression {
 public:
  Negate(Location location, AnyExpression inner) noexcept
      : Expression(location), inner_(std::move(inner)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression inner_;
};

class LogicalNot : public Expression {
 public:
  LogicalNot(Location location, AnyExpression inner) noexcept
      : Expression(location), inner_(std::move(inner)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression inner_;
};

class BitwiseNot : public Expression {
 public:
  BitwiseNot(Location location, AnyExpression inner) noexcept
      : Expression(location), inner_(std::move(inner)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression inner_;
};

class Dereference : public Expression {
 public:
  Dereference(Location location, AnyExpression inner) noexcept
      : Expression(location), inner_(std::move(inner)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression inner_;
};

class Add : public Expression {
 public:
  Add(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class Subtract : public Expression {
 public:
  Subtract(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class Multiply : public Expression {
 public:
  Multiply(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class Divide : public Expression {
 public:
  Divide(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class Modulo : public Expression {
 public:
  Modulo(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class LessThan : public Expression {
 public:
  LessThan(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class LessOrEqual : public Expression {
 public:
  LessOrEqual(Location location, AnyExpression left,
              AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class GreaterThan : public Expression {
 public:
  GreaterThan(Location location, AnyExpression left,
              AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class GreaterOrEqual : public Expression {
 public:
  GreaterOrEqual(Location location, AnyExpression left,
                 AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class Equal : public Expression {
 public:
  Equal(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class NotEqual : public Expression {
 public:
  NotEqual(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class LogicalAnd : public Expression {
 public:
  LogicalAnd(Location location, AnyExpression left,
             AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class LogicalOr : public Expression {
 public:
  LogicalOr(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class BitwiseAnd : public Expression {
 public:
  BitwiseAnd(Location location, AnyExpression left,
             AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class BitwiseOr : public Expression {
 public:
  BitwiseOr(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class BitwiseXor : public Expression {
 public:
  BitwiseXor(Location location, AnyExpression left,
             AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class ShiftLeft : public Expression {
 public:
  ShiftLeft(Location location, AnyExpression left, AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class ShiftRight : public Expression {
 public:
  ShiftRight(Location location, AnyExpression left,
             AnyExpression right) noexcept
      : Expression(location),
        left_(std::move(left)),
        right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class TernaryExpression : public Expression {
 public:
  TernaryExpression(Location location, AnyExpression condition,
                    AnyExpression then_branch,
                    AnyExpression else_branch) noexcept
      : Expression(location),
        condition_(std::move(condition)),
        then_branch_(std::move(then_branch)),
        else_branch_(std::move(else_branch)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression condition_, then_branch_, else_branch_;
};

class Statement {
 public:
  Statement(Location location) noexcept : location_(location) {}

  virtual ~Statement() = default;
  virtual void Print(std::ostream& output) const noexcept = 0;
  const Location& location() const noexcept { return location_; }

 private:
  Location location_;
};

class AnyStatement {
 public:
  // Implicit conversion from any type of expression.
  template <typename T>
  AnyStatement(T&& value)
      : value_(new std::decay_t<T>(std::forward<T>(value))) {}
  void Print(std::ostream& output) const noexcept { value_->Print(output); }
  const Location& location() const noexcept { return value_->location(); }

  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  std::unique_ptr<Statement> value_;
};

std::ostream& operator<<(std::ostream&, const AnyStatement&) noexcept;

class DeclareScalar : public Statement {
 public:
  DeclareScalar(Location location, std::string_view name) noexcept
      : Statement(location), name_(name) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  std::string name_;
};

class DeclareArray : public Statement {
 public:
  DeclareArray(Location location, std::string_view name,
               AnyExpression size) noexcept
      : Statement(location), name_(name), size_(std::move(size)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  std::string name_;
  AnyExpression size_;
};

class Assign : public Statement {
 public:
  Assign(Location location, AnyExpression left, AnyExpression right) noexcept
      : Statement(location), left_(std::move(left)), right_(std::move(right)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression left_, right_;
};

class If : public Statement {
 public:
  If(Location location, AnyExpression condition,
     std::vector<AnyStatement> then_branch,
     std::vector<AnyStatement> else_branch) noexcept
      : Statement(location),
        condition_(std::move(condition)),
        then_branch_(std::move(then_branch)),
        else_branch_(std::move(else_branch)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression condition_;
  std::vector<AnyStatement> then_branch_, else_branch_;
};

class While : public Statement {
 public:
  While(Location location, AnyExpression condition,
        std::vector<AnyStatement> body) noexcept
      : Statement(location),
        condition_(std::move(condition)),
        body_(std::move(body)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  AnyExpression condition_;
  std::vector<AnyStatement> body_;
};

class Return : public Statement {
 public:
  Return(Location location,
         std::optional<AnyExpression> value = std::nullopt) noexcept
      : Statement(location), value_(std::move(value)) {}
  void Print(std::ostream& output) const noexcept override;

 private:
  std::optional<AnyExpression> value_;
};

class Break : public Statement {
 public:
  Break(Location location) noexcept : Statement(location) {}
  void Print(std::ostream& output) const noexcept override;
};

class Continue : public Statement {
 public:
  Continue(Location location) noexcept : Statement(location) {}
  void Print(std::ostream& output) const noexcept override;
};

}  // namespace aoc2021

#endif  // AST_H_
