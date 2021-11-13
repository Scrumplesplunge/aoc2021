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

std::optional<std::int64_t> Evaluate(const ir::AnyExpression& expression);

class Evaluator {
 public:
  std::optional<std::int64_t> operator()(const ir::Label& x) {
    return std::nullopt;
  }
  std::optional<std::int64_t> operator()(const ir::Global& x) {
    return std::nullopt;
  }
  std::optional<std::int64_t> operator()(const ir::Local& x) {
    return std::nullopt;
  }
  std::optional<std::int64_t> operator()(const ir::Load64& x) {
    return std::nullopt;
  }
  std::optional<std::int64_t> operator()(const ir::IntegerLiteral& x) {
    return x.value;
  }
  std::optional<std::int64_t> operator()(const ir::Negate& x) {
    if (auto inner = Evaluate(x.inner)) {
      return -*inner;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::LogicalNot& x) {
    if (auto inner = Evaluate(x.inner)) {
      return !*inner;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::BitwiseNot& x) {
    if (auto inner = Evaluate(x.inner)) {
      return ~*inner;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::Add& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left + *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::Subtract& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left - *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::Multiply& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left * *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::Divide& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left / *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::Modulo& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left % *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::LessThan& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left < *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::LessOrEqual& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left <= *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::Equal& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left == *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::NotEqual& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left != *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::BitwiseAnd& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left & *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::BitwiseOr& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left | *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::BitwiseXor& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left ^ *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::ShiftLeft& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      return *left << *right;
    } else {
      return std::nullopt;
    }
  }
  std::optional<std::int64_t> operator()(const ir::ShiftRight& x) {
    const auto left = Evaluate(x.left), right = Evaluate(x.right);
    if (left && right) {
      // >> isn't guaranteed to do arithmetic shifting in C++, so instead we
      // convert it into a division that has the same effect as the desired
      // arithmetic shift.
      return *left / (1 << *right);
    } else {
      return std::nullopt;
    }
  }

 private:
  std::ostream* output_;
};

std::optional<std::int64_t> Evaluate(const ir::AnyExpression& expression) {
  Evaluator evaluator;
  return std::visit(evaluator, expression->value);
}

class Context {
 public:
  ir::Label Label(std::string_view prefix) {
    return ir::Label(prefix, next_label_index_++);
  }

  ir::Global Global(std::string_view prefix, std::int64_t size) {
    const ir::Global result(prefix, next_global_offset_);
    next_global_offset_ += size;
    return result;
  }

  void SetMain(ir::Label label) { main_ = std::move(label); }
  const std::optional<ir::Label>& Main() const { return main_; }

 private:
  std::optional<ir::Label> main_;
  std::int64_t next_label_index_ = 0;
  std::int64_t next_global_offset_ = 0;
};

class FrameAllocator {
 public:
  FrameAllocator() noexcept = default;
  // This takes a pointer to avoid ambiguity with a copy constructor, but the
  // pointer should never be null.
  FrameAllocator(FrameAllocator* parent) noexcept
      : parent_(parent), size_(parent->size_), max_size_(parent->max_size_) {}

  ir::Local::Offset Allocate(std::int64_t size) noexcept {
    size_ += size;
    if (size_ > max_size_) {
      max_size_ = size_;
      for (FrameAllocator* i = this;
           i->parent_ && i->parent_->max_size_ < i->max_size_; i = i->parent_) {
        i->parent_->max_size_ = i->max_size_;
      }
    }
    return ir::Local::Offset{-size_};
  }

  std::int64_t max_size() const noexcept { return max_size_; }

 private:
  FrameAllocator* parent_ = nullptr;
  std::int64_t size_ = 0;
  std::int64_t max_size_ = 0;
};

struct Array { ir::AnyExpression address; };
using Value =
    std::variant<std::int64_t, ir::Label, ir::Global, ir::Local, Array>;

ir::AnyExpression AsAddress(Location location, std::int64_t) {
  throw Error(location, "constant is not an lvalue");
}

ir::AnyExpression AsAddress(Location location, ir::Label x) { return ir::AnyExpression(x); }
ir::AnyExpression AsAddress(Location location, ir::Global x) { return x; }
ir::AnyExpression AsAddress(Location location, ir::Local x) { return x; }
ir::AnyExpression AsAddress(Location location, Array) {
  // TODO: Make arrays a proper value type which does deep copying.
  throw Error(location, "array is not an lvalue");
}

ir::AnyExpression AsAddress(Location location, const Value& v) {
  return std::visit([&](auto& x) { return AsAddress(location, x); }, v);
}

ir::AnyExpression AsValue(Location location, std::int64_t x) {
  return ir::IntegerLiteral(x);
}

ir::AnyExpression AsValue(Location location, ir::Label x) {
  return x;
}

ir::AnyExpression AsValue(Location location, ir::Global x) {
  return ir::Load64(x);
}

ir::AnyExpression AsValue(Location location, ir::Local x) {
  return ir::Load64(x);
}

ir::AnyExpression AsValue(Location location, Array x) { return x.address; }

ir::AnyExpression AsValue(Location location, const Value& v) {
  return std::visit([&](auto& x) { return AsValue(location, x); }, v);
}

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

  void SetBreak(ir::Label label) { break_ = label; }
  void SetContinue(ir::Label label) { continue_ = label; }

  const ir::Label* Break() const noexcept {
    return break_ ? &*break_ : parent_ ? parent_->Break() : nullptr;
  }

  const ir::Label* Continue() const noexcept {
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
  std::optional<ir::Label> break_, continue_;
};

struct ExpressionInfo {
  ir::AnyCode code = ir::Sequence{};
  ir::AnyExpression value;
};

class ExpressionChecker {
 public:
  ExpressionChecker(Context& context, Environment& environment,
                    FrameAllocator& frame) noexcept
      : context_(&context), environment_(&environment), frame_(&frame) {}
  ExpressionInfo operator()(const ast::Name&);
  ExpressionInfo operator()(const ast::IntegerLiteral&);
  ExpressionInfo operator()(const ast::Call&);
  ExpressionInfo operator()(const ast::Index&);
  ExpressionInfo operator()(const ast::Negate&);
  ExpressionInfo operator()(const ast::LogicalNot&);
  ExpressionInfo operator()(const ast::BitwiseNot&);
  ExpressionInfo operator()(const ast::Dereference&);
  ExpressionInfo operator()(const ast::Add&);
  ExpressionInfo operator()(const ast::Subtract&);
  ExpressionInfo operator()(const ast::Multiply&);
  ExpressionInfo operator()(const ast::Divide&);
  ExpressionInfo operator()(const ast::Modulo&);
  ExpressionInfo operator()(const ast::LessThan&);
  ExpressionInfo operator()(const ast::LessOrEqual&);
  ExpressionInfo operator()(const ast::GreaterThan&);
  ExpressionInfo operator()(const ast::GreaterOrEqual&);
  ExpressionInfo operator()(const ast::Equal&);
  ExpressionInfo operator()(const ast::NotEqual&);
  ExpressionInfo operator()(const ast::LogicalAnd&);
  ExpressionInfo operator()(const ast::LogicalOr&);
  ExpressionInfo operator()(const ast::BitwiseAnd&);
  ExpressionInfo operator()(const ast::BitwiseOr&);
  ExpressionInfo operator()(const ast::BitwiseXor&);
  ExpressionInfo operator()(const ast::ShiftLeft&);
  ExpressionInfo operator()(const ast::ShiftRight&);
  ExpressionInfo operator()(const ast::TernaryExpression&);

 private:
  ExpressionInfo CheckValue(const ast::Expression& expression);

  Context* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

ExpressionInfo CheckValue(Context& context, Environment& environment,
                          FrameAllocator& frame,
                          const ast::Expression& expression) {
  ExpressionChecker checker(context, environment, frame);
  return std::visit(checker, expression->value);
}

class AddressChecker {
 public:
  AddressChecker(Context& context, Environment& environment,
                 FrameAllocator& frame) noexcept
      : context_(&context), environment_(&environment), frame_(&frame) {}
  ExpressionInfo operator()(const ast::Name&);
  ExpressionInfo operator()(const ast::Index&);
  ExpressionInfo operator()(const ast::Dereference&);
  ExpressionInfo operator()(const auto& x) {
    throw Error(x.location, "expression is not an lvalue");
  }

 private:
  ExpressionInfo CheckAddress(const ast::Expression& expression);
  ExpressionInfo CheckValue(const ast::Expression& expression);

  Context* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

ExpressionInfo CheckAddress(Context& context, Environment& environment,
                            FrameAllocator& frame,
                            const ast::Expression& expression) {
  AddressChecker checker(context, environment, frame);
  return std::visit(checker, expression->value);
}

class StatementChecker {
 public:
  StatementChecker(Context& context, Environment& environment,
                   FrameAllocator& frame) noexcept
      : context_(&context), environment_(&environment), frame_(&frame) {}
  ir::AnyCode operator()(const ast::DeclareScalar&);
  ir::AnyCode operator()(const ast::DeclareArray&);
  ir::AnyCode operator()(const ast::Assign&);
  ir::AnyCode operator()(const ast::If&);
  ir::AnyCode operator()(const ast::While&);
  ir::AnyCode operator()(const ast::Return&);
  ir::AnyCode operator()(const ast::Break&);
  ir::AnyCode operator()(const ast::Continue&);
  ir::AnyCode operator()(const ast::DiscardedExpression&);
  ir::AnyCode operator()(const ast::FunctionDefinition&);

 private:
  ExpressionInfo CheckAddress(const ast::Expression& expression);
  ExpressionInfo CheckValue(const ast::Expression& expression);
  ir::AnyCode CheckBlock(Environment& parent_environment,
                         std::span<const ast::AnyStatement> block);
  ir::AnyCode CheckBlock(std::span<const ast::AnyStatement> block);

  Context* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

class ModuleStatementChecker {
 public:
  ModuleStatementChecker(Context& context, Environment& environment) noexcept
      : context_(&context), environment_(&environment) {}
  ir::AnyCode operator()(const ast::DeclareScalar&);
  ir::AnyCode operator()(const ast::DeclareArray&);
  ir::AnyCode operator()(const ast::Assign&);
  ir::AnyCode operator()(const ast::If&);
  ir::AnyCode operator()(const ast::While&);
  ir::AnyCode operator()(const ast::Return&);
  ir::AnyCode operator()(const ast::Break&);
  ir::AnyCode operator()(const ast::Continue&);
  ir::AnyCode operator()(const ast::DiscardedExpression&);
  ir::AnyCode operator()(const ast::FunctionDefinition&);

 private:
  ExpressionInfo CheckAddress(const ast::Expression& expression);
  ExpressionInfo CheckValue(const ast::Expression& expression);

  Context* context_;
  Environment* environment_;
};

ExpressionInfo ExpressionChecker::operator()(const ast::Name& x) {
  auto* definition = environment_->Lookup(x.value);
  if (!definition) throw UndeclaredError(x.value, x.location);
  return ExpressionInfo{.value = AsValue(x.location, definition->value)};
}

ExpressionInfo ExpressionChecker::operator()(const ast::IntegerLiteral& x) {
  return ExpressionInfo{.value = ir::IntegerLiteral(x.value)};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Call& x) {
  std::vector<ir::AnyCode> code;
  ExpressionInfo function = CheckValue(x.function);
  code.push_back(std::move(function.code));
  // TODO: Check the number of arguments for the function once types are
  // tracked.
  std::vector<ir::AnyExpression> arguments;
  for (const auto& argument : x.arguments) {
    ExpressionInfo result = CheckValue(argument);
    code.push_back(std::move(result.code));
    arguments.push_back(std::move(result.value));
  }
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(1);
  code.push_back(ir::StoreCall64(ir::Local(offset), std::move(function.value),
                                 std::move(arguments)));
  return ExpressionInfo{.code = ir::Sequence{std::move(code)},
                        .value = ir::Load64(ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Index& x) {
  ExpressionInfo container = CheckValue(x.container);
  ExpressionInfo index = CheckValue(x.index);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(container.code), std::move(index.code)}),
      .value = ir::Load64(ir::Add(
          std::move(container.value),
          ir::Multiply(ir::IntegerLiteral(8), std::move(index.value))))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Negate& x) {
  ExpressionInfo inner = CheckValue(x.inner);
  return ExpressionInfo{.code = std::move(inner.code),
                        .value = ir::Negate(std::move(inner.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalNot& x) {
  ExpressionInfo inner = CheckValue(x.inner);
  return ExpressionInfo{.code = std::move(inner.code),
                        .value = ir::LogicalNot(std::move(inner.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseNot& x) {
  ExpressionInfo inner = CheckValue(x.inner);
  return ExpressionInfo{.code = std::move(inner.code),
                        .value = ir::BitwiseNot(std::move(inner.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Dereference& x) {
  ExpressionInfo inner = CheckValue(x.inner);
  return ExpressionInfo{.code = std::move(inner.code),
                        .value = ir::Load64(std::move(inner.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Add& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::Add(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Subtract& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::Subtract(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Multiply& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::Multiply(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Divide& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::Divide(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Modulo& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::Modulo(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LessThan& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::LessThan(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LessOrEqual& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::LessOrEqual(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::GreaterThan& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  // GreaterThan(left, right) is translated into LessThan(right, left).
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::LessThan(std::move(right.value), std::move(left.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::GreaterOrEqual& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  // GreaterOrEqual(left, right) is translated into LessOrEqual(right, left).
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::LessOrEqual(std::move(right.value), std::move(left.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Equal& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::Equal(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::NotEqual& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::NotEqual(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalAnd& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(1);
  const ir::Label end = context_->Label("logical_end");
  // a && b compiles into:
  //   temp = a
  //   if (!temp) goto end
  //   temp = b
  //  end:
  //   yield temp
  return ExpressionInfo{
      .code = ir::Sequence(
          {std::move(left.code),
           ir::Store64(ir::Local(offset), std::move(left.value)),
           ir::JumpUnless(ir::Load64(ir::Local(offset)), end),
           std::move(right.code),
           ir::Store64(ir::Local(offset), std::move(right.value)), end}),
      .value = ir::Load64(ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalOr& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(1);
  const ir::Label end = context_->Label("logical_or_end");
  // a || b compiles into:
  //   temp = a
  //   if (temp) goto end
  //   temp = b
  //  end:
  //   yield temp
  return ExpressionInfo{
      .code = ir::Sequence(
          {std::move(left.code),
           ir::Store64(ir::Local(offset), std::move(left.value)),
           ir::JumpIf(ir::Load64(ir::Local(offset)), end),
           std::move(right.code),
           ir::Store64(ir::Local(offset), std::move(right.value)), end}),
      .value = ir::Load64(ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseAnd& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::BitwiseAnd(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseOr& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::BitwiseOr(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseXor& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::BitwiseXor(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftLeft& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::ShiftLeft(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftRight& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = ir::ShiftRight(std::move(left.value), std::move(right.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::TernaryExpression& x) {
  ExpressionInfo condition = CheckValue(x.condition);
  ExpressionInfo then_branch = CheckValue(x.then_branch);
  ExpressionInfo else_branch = CheckValue(x.else_branch);
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(1);
  const ir::Label if_false = context_->Label("ternary_else");
  const ir::Label end = context_->Label("ternary_end");
  // cond ? a : b compiles into:
  //   cond
  //   if (!cond) goto if_false
  //   result = a
  //   goto end
  //  if_false:
  //   result = a
  //  end:
  //   yield result
  return ExpressionInfo{
      .code = ir::Sequence(
          {std::move(condition.code),
           ir::JumpUnless(std::move(condition.value), if_false),
           std::move(then_branch.code),
           ir::Store64(ir::Local(offset), std::move(then_branch.value)),
           ir::Jump(end), if_false, std::move(else_branch.code),
           ir::Store64(ir::Local(offset), std::move(else_branch.value)), end}),
      .value = ir::Load64(ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::CheckValue(const ast::Expression& x) {
  return aoc2021::CheckValue(*context_, *environment_, *frame_, x);
}

ExpressionInfo AddressChecker::operator()(const ast::Name& x) {
  auto* definition = environment_->Lookup(x.value);
  if (!definition) throw UndeclaredError(x.value, x.location);
  return ExpressionInfo{.value = AsAddress(x.location, definition->value)};
}

ExpressionInfo AddressChecker::operator()(const ast::Index& x) {
  ExpressionInfo container = CheckAddress(x.container);
  ExpressionInfo index = CheckValue(x.index);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(container.code), std::move(index.code)}),
      .value =
          ir::Add(std::move(container.value),
                  ir::Multiply(ir::IntegerLiteral(8), std::move(index.value)))};
}

ExpressionInfo AddressChecker::operator()(const ast::Dereference& x) {
  ExpressionInfo inner = CheckValue(x.inner);
  return ExpressionInfo{.code = std::move(inner.code),
                        .value = std::move(inner.value)};
}

ExpressionInfo AddressChecker::CheckAddress(const ast::Expression& x) {
  return aoc2021::CheckAddress(*context_, *environment_, *frame_, x);
}

ExpressionInfo AddressChecker::CheckValue(const ast::Expression& x) {
  return aoc2021::CheckValue(*context_, *environment_, *frame_, x);
}

ir::AnyCode CheckBlock(Context& context, Environment& parent_environment,
                       FrameAllocator& parent_frame,
                       std::span<const ast::AnyStatement> block) {
  std::vector<ir::AnyCode> code;
  FrameAllocator frame(&parent_frame);
  Environment environment(parent_environment, Environment::ShadowMode::kDeny);
  for (const auto& statement : block) {
    StatementChecker checker(context, environment, frame);
    code.push_back(std::visit(checker, statement->value));
  }
  return ir::Sequence(std::move(code));
}

ir::AnyCode StatementChecker::operator()(const ast::DeclareScalar& x) {
  const ir::Local::Offset offset = frame_->Allocate(1);
  environment_->Define(x.name,
                       Environment::Definition{.location = x.location,
                                               .value = ir::Local(offset)});
  return ir::Sequence();
}

ir::AnyCode StatementChecker::operator()(const ast::DeclareArray& x) {
  auto size = Evaluate(CheckValue(x.size).value);
  if (!size) {
    throw Error(x.location, "array size must be a constant expression");
  }
  const ir::Local::Offset offset = frame_->Allocate(*size);
  environment_->Define(
      x.name, Environment::Definition{.location = x.location,
                                      .value = Array(ir::Local(offset))});
  return ir::Sequence();
}

ir::AnyCode StatementChecker::operator()(const ast::Assign& x) {
  ExpressionInfo left = CheckAddress(x.left);
  ExpressionInfo right = CheckValue(x.right);
  return ir::Sequence(
      {std::move(left.code), std::move(right.code),
       ir::Store64(std::move(left.value), std::move(right.value))});
}

ir::AnyCode StatementChecker::operator()(const ast::If& x) {
  ExpressionInfo condition = CheckValue(x.condition);
  ir::AnyCode then_branch = CheckBlock(x.then_branch);
  ir::AnyCode else_branch = CheckBlock(x.else_branch);
  const ir::Label if_false = context_->Label("if_false");
  const ir::Label end = context_->Label("if_end");
  return ir::Sequence({std::move(condition.code),
                       ir::JumpUnless(std::move(condition.value), if_false),
                       std::move(then_branch), ir::Jump(end), if_false,
                       std::move(else_branch), end});
}

ir::AnyCode StatementChecker::operator()(const ast::While& x) {
  const ir::Label loop_start = context_->Label("while_start");
  const ir::Label loop_condition = context_->Label("while_condition");
  const ir::Label loop_end = context_->Label("while_end");
  ExpressionInfo condition = CheckValue(x.condition);
  Environment while_environment(*environment_, Environment::ShadowMode::kDeny);
  while_environment.SetBreak(loop_end);
  while_environment.SetContinue(loop_condition);
  ir::AnyCode body = CheckBlock(while_environment, x.body);
  return ir::Sequence({ir::Jump(loop_condition), loop_start, std::move(body),
                       loop_condition, std::move(condition.code),
                       ir::JumpIf(std::move(condition.value), loop_start),
                       loop_end});
}

ir::AnyCode StatementChecker::operator()(const ast::Return& x) {
  if (x.value) {
    ExpressionInfo value = CheckValue(*x.value);
    return ir::Sequence(
        {std::move(value.code), ir::Return(std::move(value.value))});
  } else {
    return ir::Return(ir::IntegerLiteral(0));
  }
}

ir::AnyCode StatementChecker::operator()(const ast::Break& x) {
  auto label = environment_->Break();
  if (!label) {
    throw Error(x.location, "break statement outside of a breakable context");
  }
  return ir::Jump(*label);
}

ir::AnyCode StatementChecker::operator()(const ast::Continue& x) {
  auto label = environment_->Break();
  if (!label) {
    throw Error(x.location, "continue statement outside of a loop");
  }
  return ir::Jump(*label);
}

ir::AnyCode StatementChecker::operator()(const ast::DiscardedExpression& x) {
  return CheckValue(x.expression).code;
}

ir::AnyCode StatementChecker::operator()(const ast::FunctionDefinition& x) {
  throw Error(x.location, "nested function definitions are forbidden");
}

ExpressionInfo StatementChecker::CheckAddress(
    const ast::Expression& x) {
  return aoc2021::CheckAddress(*context_, *environment_, *frame_, x);
}

ExpressionInfo StatementChecker::CheckValue(const ast::Expression& x) {
  return aoc2021::CheckValue(*context_, *environment_, *frame_, x);
}

ir::AnyCode StatementChecker::CheckBlock(
    Environment& parent_environment, std::span<const ast::AnyStatement> block) {
  return aoc2021::CheckBlock(*context_, parent_environment, *frame_, block);
}

ir::AnyCode StatementChecker::CheckBlock(
    std::span<const ast::AnyStatement> block) {
  return CheckBlock(*environment_, block);
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::DeclareScalar& x) {
  const ir::Global global = context_->Global(x.name, 1);
  environment_->Define(
      x.name, Environment::Definition{.location = x.location, .value = global});
  return ir::Sequence();
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::DeclareArray& x) {
  auto size = Evaluate(CheckValue(x.size).value);
  if (!size) {
    throw Error(x.location, "array size must be a constant expression");
  }
  const ir::Global global = context_->Global(x.name, *size);
  environment_->Define(x.name, Environment::Definition{.location = x.location,
                                                       .value = Array(global)});
  return ir::Sequence();
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::Assign& x) {
  throw Error(x.location,
              "assignment statements are forbidden at module scope");
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::If& x) {
  throw Error(x.location,
              "if statements are forbidden at module scope");
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::While& x) {
  throw Error(x.location,
              "while statements are forbidden at module scope");
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::Return& x) {
  throw Error(x.location, "return statement outside of a function");
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::Break& x) {
  throw Error(x.location, "break statement outside of a breakable context");
}

ir::AnyCode ModuleStatementChecker::operator()(const ast::Continue& x) {
  throw Error(x.location, "continue statement outside of a loop");
}

ir::AnyCode ModuleStatementChecker::operator()(
    const ast::DiscardedExpression& x) {
  throw Error(x.location,
              "discarded expressions are forbidden at module scope");
}

ir::AnyCode ModuleStatementChecker::operator()(
    const ast::FunctionDefinition& x) {
  const ir::Label function = context_->Label("function");
  // TODO: Derive symbolic constant names in a better way.
  environment_->Define(x.name, Environment::Definition{.location = x.location,
                                                       .value = function});
  if (x.name == "main") context_->SetMain(function);
  Environment function_environment(*environment_,
                                   Environment::ShadowMode::kAllow);
  const int n = x.parameters.size();
  // Parameters are arranged above the function stack frame:
  //  ...
  //  arg2
  //  arg1
  //  return slot
  //  return address
  //  saved frame pointer <- frame pointer points here
  //  ...
  //  local2
  //  local1
  for (int i = 0; i < n; i++) {
    const auto& parameter = x.parameters[i];
    const ir::Local::Offset offset{i + 3};
    function_environment.Define(
        parameter.value, Environment::Definition{.location = parameter.location,
                                                 .value = ir::Local(offset)});
  }

  FrameAllocator frame;
  ir::AnyCode code = CheckBlock(*context_, function_environment, frame, x.body);
  return ir::Sequence({function, ir::BeginFrame(frame.max_size()),
                       std::move(code), ir::Return(ir::IntegerLiteral(0))});
}

ExpressionInfo ModuleStatementChecker::CheckValue(const ast::Expression& x) {
  // All expressions at global scope must be constant expressions, so the frame
  // is not used.
  FrameAllocator frame;
  return aoc2021::CheckValue(*context_, *environment_, frame, x);
}

}  // namespace

ir::Unit Check(std::span<const ast::AnyStatement> program) {
  Context context;
  Environment global;
  std::vector<ir::AnyCode> code;
  for (const auto& statement : program) {
    ModuleStatementChecker checker(context, global);
    code.push_back(std::visit(checker, statement->value));
  }
  return ir::Unit{.main = context.Main(),
                  .code = ir::Flatten(ir::Sequence(std::move(code)))};
}

}  // namespace aoc2021
