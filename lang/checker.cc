#include "checker.h"

#include "string_utils.h"

#include <map>

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

class Environment {
 public:
  enum class ShadowMode {
    kAllow,
    kDeny,
  };

  enum class Kind {
    kConstant,
    kVariable,
  };

  struct Definition {
    Location location;
    Kind kind;
  };

  Environment() noexcept : parent_(nullptr), shadow_mode_(ShadowMode::kDeny) {}
  Environment(Environment& parent, ShadowMode shadow_mode) noexcept
      : parent_(&parent), shadow_mode_(shadow_mode) {}

  void Define(std::string_view name, Definition definition) {
    if (shadow_mode_ == ShadowMode::kDeny && parent_) {
      auto* previous = parent_->Lookup(name);
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

 private:
  Environment* parent_;
  ShadowMode shadow_mode_;
  std::map<std::string, Definition, std::less<>> names_;
  std::optional<Location> break_, continue_;
};

class ExpressionChecker : public ast::ExpressionVisitor<void> {
 public:
  ExpressionChecker(Environment& environment) noexcept
      : environment_(&environment) {}
  void operator()(const ast::Name&) override;
  void operator()(const ast::IntegerLiteral&) override;
  void operator()(const ast::Call&) override;
  void operator()(const ast::Index&) override;
  void operator()(const ast::Negate&) override;
  void operator()(const ast::LogicalNot&) override;
  void operator()(const ast::BitwiseNot&) override;
  void operator()(const ast::Dereference&) override;
  void operator()(const ast::Add&) override;
  void operator()(const ast::Subtract&) override;
  void operator()(const ast::Multiply&) override;
  void operator()(const ast::Divide&) override;
  void operator()(const ast::Modulo&) override;
  void operator()(const ast::LessThan&) override;
  void operator()(const ast::LessOrEqual&) override;
  void operator()(const ast::GreaterThan&) override;
  void operator()(const ast::GreaterOrEqual&) override;
  void operator()(const ast::Equal&) override;
  void operator()(const ast::NotEqual&) override;
  void operator()(const ast::LogicalAnd&) override;
  void operator()(const ast::LogicalOr&) override;
  void operator()(const ast::BitwiseAnd&) override;
  void operator()(const ast::BitwiseOr&) override;
  void operator()(const ast::BitwiseXor&) override;
  void operator()(const ast::ShiftLeft&) override;
  void operator()(const ast::ShiftRight&) override;
  void operator()(const ast::TernaryExpression&) override;

 private:
  template <typename T> void CheckUnary(const T&);
  template <typename T> void CheckBinary(const T&);

  Environment* environment_;
};

void Check(Environment& environment, const ast::AnyExpression& expression) {
  ExpressionChecker checker(environment);
  expression.Visit(checker);
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

void ExpressionChecker::operator()(const ast::Name& x) {
  auto* definition = environment_->Lookup(x.value);
  if (!definition) throw UndeclaredError(x.value, x.location);
}

void ExpressionChecker::operator()(const ast::IntegerLiteral& x) {}

void ExpressionChecker::operator()(const ast::Call& x) {
  Check(*environment_, x.function);
  // TODO: Check the number of arguments for the function once types are
  // tracked.
  for (const auto& argument : x.arguments) Check(*environment_, argument);
}

void ExpressionChecker::operator()(const ast::Index& x) {
  Check(*environment_, x.container);
  Check(*environment_, x.index);
}

void ExpressionChecker::operator()(const ast::Negate& x) { CheckUnary(x); }
void ExpressionChecker::operator()(const ast::LogicalNot& x) { CheckUnary(x); }
void ExpressionChecker::operator()(const ast::BitwiseNot& x) { CheckUnary(x); }
void ExpressionChecker::operator()(const ast::Dereference& x) { CheckUnary(x); }
void ExpressionChecker::operator()(const ast::Add& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::Subtract& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::Multiply& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::Divide& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::Modulo& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::LessThan& x) { CheckBinary(x); }

void ExpressionChecker::operator()(const ast::LessOrEqual& x) {
  CheckBinary(x);
}

void ExpressionChecker::operator()(const ast::GreaterThan& x) {
  CheckBinary(x);
}

void ExpressionChecker::operator()(const ast::GreaterOrEqual& x) {
  CheckBinary(x);
}

void ExpressionChecker::operator()(const ast::Equal& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::NotEqual& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::LogicalAnd& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::LogicalOr& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::BitwiseAnd& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::BitwiseOr& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::BitwiseXor& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::ShiftLeft& x) { CheckBinary(x); }
void ExpressionChecker::operator()(const ast::ShiftRight& x) { CheckBinary(x); }

void ExpressionChecker::operator()(const ast::TernaryExpression& x) {
  {
    ExpressionChecker checker(*environment_);
    x.condition.Visit(checker);
  }
  {
    ExpressionChecker checker(*environment_);
    x.then_branch.Visit(checker);
  }
  {
    ExpressionChecker checker(*environment_);
    x.else_branch.Visit(checker);
  }
}

template <typename T>
void ExpressionChecker::CheckUnary(const T& x) {
  Check(*environment_, x.inner);
}

template <typename T>
void ExpressionChecker::CheckBinary(const T& x) {
  Check(*environment_, x.left);
  Check(*environment_, x.right);
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
      x.name, Environment::Definition{.location = x.location,
                                      .kind = Environment::Kind::kVariable});
}

void StatementChecker::operator()(const ast::DeclareArray& x) {
  environment_->Define(
      x.name, Environment::Definition{.location = x.location,
                                      .kind = Environment::Kind::kVariable});
  ExpressionChecker checker(*environment_);
  // TODO: Confirm that the size expression is a constant expression.
  x.size.Visit(checker);
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
      x.name, Environment::Definition{.location = x.location,
                                      .kind = Environment::Kind::kVariable});
}

void ModuleStatementChecker::operator()(const ast::DeclareArray& x) {
  environment_->Define(
      x.name, Environment::Definition{.location = x.location,
                                      .kind = Environment::Kind::kVariable});
  // TODO: Confirm that the size expression is a constant expression.
  ExpressionChecker checker(*environment_);
  x.size.Visit(checker);
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
  environment_->Define(x.name, Environment::Definition{
                                   .location = x.location,
                                   .kind = Environment::Kind::kConstant,
                               });
  Environment function_environment(*environment_,
                                   Environment::ShadowMode::kAllow);
  for (const auto& parameter : x.parameters) {
    function_environment.Define(
        parameter.value,
        Environment::Definition{.location = parameter.location,
                                .kind = Environment::Kind::kVariable});
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
