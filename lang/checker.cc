#include "checker.h"

#include "string_utils.h"

#include <map>
#include <variant>

namespace aoc2021 {
namespace {

template <typename... Args>
CheckError Error(Location location, const Args&... args) {
  std::vector<Message> messages;
  messages.push_back(Message{
      .location = location,
      .type = Message::Type::kError,
      .text = StrCat(args...),
  });
  return CheckError(std::move(messages));
}

// An error for redeclaring a given name.
CheckError RedeclarationError(std::string_view name, Location previous,
                              Location current) {
  std::vector<Message> messages;
  messages.push_back(Message{
      .location = current,
      .type = Message::Type::kError,
      .text = StrCat("redeclaration of '", name, "'"),
  });
  messages.push_back(Message{
      .location = previous,
      .type = Message::Type::kNote,
      .text = "previous declaration was here",
  });
  return CheckError(std::move(messages));
}

// An error for using an undeclared name.
CheckError UndeclaredError(std::string_view name, Location location) {
  return Error(location, "use of undeclared name '", name, "'");
}

// Represents a value which can change at runtime.
struct DynamicValue {};

// Represents a constant value which is known at compile time.
struct Constant { std::int64_t value; };

// Represents a symbolic constant with the given name.
struct SymbolicConstant { std::string name; };

using Value = std::variant<DynamicValue, Constant, SymbolicConstant>;

class Environment {
 public:
  enum class ShadowMode {
    kAllow,
    kDeny,
  };

  struct Definition {
    Location location;
    Value value;
  };

  Environment() noexcept : parent_(nullptr), shadow_mode_(ShadowMode::kDeny) {}
  Environment(Environment& parent, ShadowMode shadow_mode) noexcept
      : parent_(&parent), shadow_mode_(shadow_mode) {}

  void Define(std::string_view name, Definition definition) {
    if (shadow_mode_ == ShadowMode::kDeny && parent_) {
      auto* previous = parent_->LookupWithinShadowDomain(name);
      if (previous) {
        throw RedeclarationError(name, previous->location, definition.location);
      }
    }
    auto [i, is_new] = names_.emplace(name, definition);
    if (!is_new) {
      throw RedeclarationError(name, i->second.location, definition.location);
    }
  }

  void SetBreak(Location location) { break_ = location; }
  void SetContinue(Location location) { continue_ = location; }

  const Location* Break() const noexcept {
    return break_ ? &*break_ : parent_ ? parent_->Break() : nullptr;
  }

  const Location* Continue() const noexcept {
    return continue_ ? &*continue_ : parent_ ? parent_->Continue() : nullptr;
  }

  const Definition* Lookup(std::string_view name) const {
    auto i = names_.find(name);
    if (i != names_.end()) return &i->second;
    return parent_ ? parent_->Lookup(name) : nullptr;
  }

  // Like Lookup(), except that it only searches within the broadest lexical
  // scope that forbids shadowing: variables outside this scope are ignored.
  const Definition* LookupWithinShadowDomain(std::string_view name) const {
    auto i = names_.find(name);
    if (i != names_.end()) return &i->second;
    return shadow_mode_ == ShadowMode::kDeny && parent_
               ? parent_->LookupWithinShadowDomain(name)
               : nullptr;
  }

 private:
  Environment* parent_;
  ShadowMode shadow_mode_;
  std::map<std::string, Definition, std::less<>> names_;
  std::optional<Location> break_, continue_;
};

struct ExpressionInfo {
  Value value;
};

class ExpressionChecker : public ast::ExpressionVisitor<ExpressionInfo> {
 public:
  ExpressionChecker(Environment& environment) noexcept
      : environment_(&environment) {}
  ExpressionInfo operator()(const ast::Name&) override;
  ExpressionInfo operator()(const ast::IntegerLiteral&) override;
  ExpressionInfo operator()(const ast::Call&) override;
  ExpressionInfo operator()(const ast::Index&) override;
  ExpressionInfo operator()(const ast::Negate&) override;
  ExpressionInfo operator()(const ast::LogicalNot&) override;
  ExpressionInfo operator()(const ast::BitwiseNot&) override;
  ExpressionInfo operator()(const ast::Dereference&) override;
  ExpressionInfo operator()(const ast::Add&) override;
  ExpressionInfo operator()(const ast::Subtract&) override;
  ExpressionInfo operator()(const ast::Multiply&) override;
  ExpressionInfo operator()(const ast::Divide&) override;
  ExpressionInfo operator()(const ast::Modulo&) override;
  ExpressionInfo operator()(const ast::LessThan&) override;
  ExpressionInfo operator()(const ast::LessOrEqual&) override;
  ExpressionInfo operator()(const ast::GreaterThan&) override;
  ExpressionInfo operator()(const ast::GreaterOrEqual&) override;
  ExpressionInfo operator()(const ast::Equal&) override;
  ExpressionInfo operator()(const ast::NotEqual&) override;
  ExpressionInfo operator()(const ast::LogicalAnd&) override;
  ExpressionInfo operator()(const ast::LogicalOr&) override;
  ExpressionInfo operator()(const ast::BitwiseAnd&) override;
  ExpressionInfo operator()(const ast::BitwiseOr&) override;
  ExpressionInfo operator()(const ast::BitwiseXor&) override;
  ExpressionInfo operator()(const ast::ShiftLeft&) override;
  ExpressionInfo operator()(const ast::ShiftRight&) override;
  ExpressionInfo operator()(const ast::TernaryExpression&) override;

 private:
  template <typename T> void CheckUnary(const T&);
  template <typename T> void CheckBinary(const T&);

  Environment* environment_;
};

ExpressionInfo Check(Environment& environment,
                     const ast::AnyExpression& expression) {
  ExpressionChecker checker(environment);
  return expression.Visit(checker);
}

class StatementChecker : public ast::StatementVisitor {
 public:
  StatementChecker(Environment& environment) noexcept
      : environment_(&environment) {}
  virtual void operator()(const ast::DeclareScalar&) override;
  virtual void operator()(const ast::DeclareArray&) override;
  virtual void operator()(const ast::Assign&) override;
  virtual void operator()(const ast::If&) override;
  virtual void operator()(const ast::While&) override;
  virtual void operator()(const ast::Return&) override;
  virtual void operator()(const ast::Break&) override;
  virtual void operator()(const ast::Continue&) override;
  virtual void operator()(const ast::DiscardedExpression&) override;
  virtual void operator()(const ast::FunctionDefinition&) override;

 private:
  Environment* environment_;
};

class ModuleStatementChecker : public ast::StatementVisitor {
 public:
  ModuleStatementChecker(Environment& environment) noexcept
      : environment_(&environment) {}
  virtual void operator()(const ast::DeclareScalar&) override;
  virtual void operator()(const ast::DeclareArray&) override;
  virtual void operator()(const ast::Assign&) override;
  virtual void operator()(const ast::If&) override;
  virtual void operator()(const ast::While&) override;
  virtual void operator()(const ast::Return&) override;
  virtual void operator()(const ast::Break&) override;
  virtual void operator()(const ast::Continue&) override;
  virtual void operator()(const ast::DiscardedExpression&) override;
  virtual void operator()(const ast::FunctionDefinition&) override;

 private:
  Environment* environment_;
};

ExpressionInfo ExpressionChecker::operator()(const ast::Name& x) {
  auto* definition = environment_->Lookup(x.value);
  if (!definition) throw UndeclaredError(x.value, x.location);
  return ExpressionInfo{.value = definition->value};
}

ExpressionInfo ExpressionChecker::operator()(const ast::IntegerLiteral& x) {
  return ExpressionInfo{.value = Constant(x.value)};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Call& x) {
  Check(*environment_, x.function);
  // TODO: Check the number of arguments for the function once types are
  // tracked.
  for (const auto& argument : x.arguments) Check(*environment_, argument);
  return ExpressionInfo{.value = DynamicValue()};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Index& x) {
  Check(*environment_, x.container);
  Check(*environment_, x.index);
  return ExpressionInfo{.value = DynamicValue()};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Negate& x) {
  ExpressionInfo inner = Check(*environment_, x.inner);
  if (auto* x = std::get_if<Constant>(&inner.value)) {
    return ExpressionInfo{.value = Constant(-x->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalNot& x) {
  ExpressionInfo inner = Check(*environment_, x.inner);
  if (auto* x = std::get_if<Constant>(&inner.value)) {
    return ExpressionInfo{.value = Constant(!x->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseNot& x) {
  ExpressionInfo inner = Check(*environment_, x.inner);
  if (auto* x = std::get_if<Constant>(&inner.value)) {
    return ExpressionInfo{.value = Constant(~x->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Dereference& x) {
  ExpressionInfo inner = Check(*environment_, x.inner);
  return ExpressionInfo{.value = DynamicValue()};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Add& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value + r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Subtract& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value - r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Multiply& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value * r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Divide& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value / r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Modulo& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value % r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::LessThan& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value < r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::LessOrEqual& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value <= r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::GreaterThan& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value > r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::GreaterOrEqual& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value >= r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Equal& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value == r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::NotEqual& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value != r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalAnd& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && !l->value) {
    // Short-circuit evaluation: it doesn't matter whether the second argument
    // is a constant expression since the first one evaluated to false.
    return ExpressionInfo{.value = Constant(false)};
  } else if (l && r) {
    return ExpressionInfo{.value = Constant(l->value && r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalOr& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && l->value) {
    // Short-circuit evaluation: it doesn't matter whether the second argument
    // is a constant expression since the first one evaluated to true.
    return ExpressionInfo{.value = Constant(true)};
  } else if (l && r) {
    return ExpressionInfo{.value = Constant(l->value || r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseAnd& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value & r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseOr& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value | r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseXor& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value ^ r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftLeft& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    return ExpressionInfo{.value = Constant(l->value << r->value)};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftRight& x) {
  ExpressionInfo left = Check(*environment_, x.left);
  ExpressionInfo right = Check(*environment_, x.right);
  auto* l = std::get_if<Constant>(&left.value);
  auto* r = std::get_if<Constant>(&right.value);
  if (l && r) {
    // >> isn't guaranteed to do arithmetic shifting in C++, so instead we
    // convert it into a division that has the same effect as the desired
    // arithmetic shift.
    return ExpressionInfo{.value = Constant(l->value / (1 << r->value))};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::TernaryExpression& x) {
  ExpressionInfo condition = Check(*environment_, x.condition);
  ExpressionInfo then_branch = Check(*environment_, x.then_branch);
  ExpressionInfo else_branch = Check(*environment_, x.else_branch);
  // The ternary expression will only be a constant expression if the condition
  // is a constant expression and the corresponding branch is also a constant
  // expression.
  if (auto* c = std::get_if<Constant>(&condition.value)) {
    return ExpressionInfo{.value =
                              c->value ? then_branch.value : else_branch.value};
  } else {
    return ExpressionInfo{.value = DynamicValue()};
  }
}

void CheckBlock(Environment& parent, std::span<const ast::AnyStatement> block) {
  Environment environment(parent, Environment::ShadowMode::kDeny);
  for (const auto& statement : block) {
    StatementChecker checker(environment);
    statement.Visit(checker);
  }
}

void StatementChecker::operator()(const ast::DeclareScalar& x) {
  environment_->Define(
      x.name,
      Environment::Definition{.location = x.location, .value = DynamicValue()});
}

void StatementChecker::operator()(const ast::DeclareArray& x) {
  environment_->Define(
      x.name,
      Environment::Definition{.location = x.location, .value = DynamicValue()});
  ExpressionChecker checker(*environment_);
  ExpressionInfo size = x.size.Visit(checker);
  if (!std::holds_alternative<Constant>(size.value)) {
    throw Error(x.location, "array size must be a constant expression");
  }
}

void StatementChecker::operator()(const ast::Assign& x) {
  {
    ExpressionChecker checker(*environment_);
    x.left.Visit(checker);
    // TODO: Confirm that the left expression is an lvalue.
  }
  {
    ExpressionChecker checker(*environment_);
    x.right.Visit(checker);
  }
}

void StatementChecker::operator()(const ast::If& x) {
  {
    ExpressionChecker checker(*environment_);
    x.condition.Visit(checker);
  }
  CheckBlock(*environment_, x.then_branch);
  CheckBlock(*environment_, x.else_branch);
}

void StatementChecker::operator()(const ast::While& x) {
  {
    ExpressionChecker checker(*environment_);
    x.condition.Visit(checker);
  }
  Environment body_environment(*environment_, Environment::ShadowMode::kDeny);
  body_environment.SetBreak(x.location);
  body_environment.SetContinue(x.location);
  CheckBlock(body_environment, x.body);
}

void StatementChecker::operator()(const ast::Return& x) {
  if (x.value) {
    ExpressionChecker checker(*environment_);
    x.value->Visit(checker);
  }
}

void StatementChecker::operator()(const ast::Break& x) {
  if (!environment_->Break()) {
    throw Error(x.location, "break statement outside of a breakable context");
  }
}

void StatementChecker::operator()(const ast::Continue& x) {
  if (!environment_->Continue()) {
    throw Error(x.location, "continue statement outside of a loop");
  }
}

void StatementChecker::operator()(const ast::DiscardedExpression& x) {
  ExpressionChecker checker(*environment_);
  x.expression.Visit(checker);
}

void StatementChecker::operator()(const ast::FunctionDefinition& x) {
  throw Error(x.location, "nested function definitions are forbidden");
}

void ModuleStatementChecker::operator()(const ast::DeclareScalar& x) {
  environment_->Define(
      x.name,
      Environment::Definition{.location = x.location, .value = DynamicValue()});
}

void ModuleStatementChecker::operator()(const ast::DeclareArray& x) {
  environment_->Define(
      x.name,
      Environment::Definition{.location = x.location, .value = DynamicValue()});
  ExpressionChecker checker(*environment_);
  ExpressionInfo size = x.size.Visit(checker);
  if (!std::holds_alternative<Constant>(size.value)) {
    throw Error(x.location, "array size must be a constant expression");
  }
}

void ModuleStatementChecker::operator()(const ast::Assign& x) {
  throw Error(x.location,
              "assignment statements are forbidden at module scope");
}

void ModuleStatementChecker::operator()(const ast::If& x) {
  throw Error(x.location,
              "if statements are forbidden at module scope");
}

void ModuleStatementChecker::operator()(const ast::While& x) {
  throw Error(x.location,
              "while statements are forbidden at module scope");
}

void ModuleStatementChecker::operator()(const ast::Return& x) {
  throw Error(x.location, "return statement outside of a function");
}

void ModuleStatementChecker::operator()(const ast::Break& x) {
  throw Error(x.location, "break statement outside of a breakable context");
}

void ModuleStatementChecker::operator()(const ast::Continue& x) {
  throw Error(x.location, "continue statement outside of a loop");
}

void ModuleStatementChecker::operator()(const ast::DiscardedExpression& x) {
  throw Error(x.location,
              "discarded expressions are forbidden at module scope");
}

void ModuleStatementChecker::operator()(const ast::FunctionDefinition& x) {
  // TODO: Derive symbolic constant names in a better way.
  environment_->Define(
      x.name, Environment::Definition{.location = x.location,
                                      .value = SymbolicConstant(x.name)});
  Environment function_environment(*environment_,
                                   Environment::ShadowMode::kAllow);
  for (const auto& parameter : x.parameters) {
    function_environment.Define(
        parameter.value, Environment::Definition{.location = parameter.location,
                                                 .value = DynamicValue()});
  }
  for (const auto& statement : x.body) {
    StatementChecker checker(function_environment);
    statement.Visit(checker);
  }
}

}  // namespace

TODO Check(std::span<const ast::AnyStatement> program) {
  Environment global;
  for (const auto& statement : program) {
    ModuleStatementChecker checker(global);
    statement.Visit(checker);
  }
  return TODO{};
}

}  // namespace aoc2021
