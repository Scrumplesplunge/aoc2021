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

std::optional<std::int64_t> Evaluate(const ir::Expression& expression);

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

std::optional<std::int64_t> Evaluate(const ir::Expression& expression) {
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
    globals_.emplace(result, size);
    return result;
  }

  void SetMain(ir::Label label) { main_ = std::move(label); }
  const std::optional<ir::Label>& Main() const { return main_; }

  const std::map<ir::Global, std::int64_t>& Globals() const { return globals_; }

 private:
  std::optional<ir::Label> main_;
  std::int64_t next_label_index_ = 0;
  std::map<ir::Global, std::int64_t> globals_;
  std::int64_t next_global_offset_ = 0;
};

class FrameAllocator {
 public:
  FrameAllocator() noexcept = default;
  // This takes a pointer to avoid ambiguity with a copy constructor, but the
  // pointer should never be null.
  FrameAllocator(FrameAllocator* parent) noexcept
      : parent_(parent), size_(parent->size_), max_size_(parent->max_size_) {}

  ir::Local::Offset Allocate(std::int64_t size,
                             std::int64_t alignment) noexcept {
    if (size == 0) return ir::Local::Offset{0};
    // Pad to the alignment requirement.
    size_ = (size_ + (alignment - 1)) / alignment * alignment;
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

  ir::Local::Offset Allocate(const ir::Type& type) noexcept {
    return Allocate(Size(type), Alignment(type));
  }

  std::int64_t max_size() const noexcept { return max_size_; }

 private:
  FrameAllocator* parent_ = nullptr;
  std::int64_t size_ = 0;
  std::int64_t max_size_ = 0;
};

enum class Category {
  kLvalue,
  kRvalue,
};

enum class Representation {
  kDirect,
  kAddress,
};

struct TypedExpression {
  Category category;
  ir::Type type;
  Representation representation;
  ir::Expression value;
};

struct ExpressionInfo {
  ir::Code code = ir::Sequence{};
  TypedExpression value;
};

using Value = std::variant<TypedExpression, ir::Type>;

TypedExpression AsValue(Location location, const TypedExpression& x) {
  return x;
}

TypedExpression AsValue(Location location, const ir::Type& x) {
  throw Error(location, "type used in a value context");
}

TypedExpression AsValue(Location location, const Value& v) {
  return std::visit([&](auto& x) { return AsValue(location, x); }, v);
}

ir::Type AsType(Location location, const ir::Type& x) { return x; }
ir::Type AsType(Location location, const auto& x) {
  throw Error(location, "value used in type context");
}
ir::Type AsType(Location location, const Value& v) {
  return std::visit([&](auto& x) { return AsType(location, x); }, v);
}

ExpressionInfo EnsureRvalue(Location location, FrameAllocator& frame,
                            ExpressionInfo info) {
  if (info.value.category != Category::kRvalue) {
    const ir::Local::Offset copy = frame.Allocate(info.value.type);
    if (info.value.representation != Representation::kAddress) std::abort();
    return ExpressionInfo{
        .code = ir::Call(ir::Label("copy"),
                         {ir::Local(copy), info.value.value,
                          ir::IntegerLiteral(ir::Size(info.value.type))}),
        .value = TypedExpression{.category = Category::kRvalue,
                                 .type = info.value.type,
                                 .representation = Representation::kAddress,
                                 .value = ir::Local(copy)}};
  }
  return info;
}

ExpressionInfo EnsureInt64(Location location, ExpressionInfo info) {
  if (info.value.type != ir::Primitive::kInt64) {
    throw Error(location, "not an integer");
  }
  if (info.value.representation == Representation::kDirect) {
    return info;
  } else {
    return ExpressionInfo{
        .code = std::move(info.code),
        .value =
            TypedExpression{.category = Category::kRvalue,
                            .type = std::move(info.value.type),
                            .representation = Representation::kDirect,
                            .value = ir::Load64(std::move(info.value.value))}};
  }
}

ExpressionInfo EnsureComparable(Location location, ExpressionInfo info) {
  if (std::get_if<ir::Array>(&info.value.type->value)) {
    throw Error(location, "arrays are not comparable");
  }
  if (info.value.representation == Representation::kDirect) {
    return info;
  } else {
    return ExpressionInfo{
        .code = std::move(info.code),
        .value =
            TypedExpression{.category = Category::kRvalue,
                            .type = std::move(info.value.type),
                            .representation = Representation::kDirect,
                            .value = ir::Load64(std::move(info.value.value))}};
  }
}

ExpressionInfo ConvertTo(Location location, const ir::Type& target,
                         ExpressionInfo info) {
  if (info.value.type == target) return info;
  // *[n]T -> []T
  {
    const auto* s = std::get_if<ir::Span>(&target->value);
    const auto* p = std::get_if<ir::Pointer>(&info.value.type->value);
    const auto* a = p ? std::get_if<ir::Array>(&p->pointee->value) : nullptr;
    if (s && a && s->element == a->element) {
      info.value.type = target;
      return info;
    }
  }
  throw Error(location, "cannot implicitly convert ", info.value.type, " to ",
              target);
}

const ir::FunctionPointer& AsFunctionPointer(
    Location location, const ir::FunctionPointer& x) {
  return x;
}

const ir::FunctionPointer& AsFunctionPointer(Location location,
                                             const auto& x) {
  throw Error(location, "trying to use ", x, " as a function");
}

const ir::FunctionPointer& AsFunctionPointer(Location location,
                                             const ir::Type& x) {
  return std::visit([&](const auto& x) -> const ir::FunctionPointer& {
    return AsFunctionPointer(location, x);
  }, x->value);
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
  ExpressionInfo operator()(const ast::AddressOf&);
  ExpressionInfo operator()(const ast::Add&);
  ExpressionInfo operator()(const ast::Subtract&);
  ExpressionInfo operator()(const ast::Multiply&);
  ExpressionInfo operator()(const ast::Divide&);
  ExpressionInfo operator()(const ast::Modulo&);
  ExpressionInfo operator()(const ast::As&);
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
  ExpressionInfo operator()(const ast::ArrayType&);
  ExpressionInfo operator()(const ast::SpanType&);
  ExpressionInfo operator()(const ast::TernaryExpression&);

 private:
  ExpressionInfo CheckValue(const ast::Expression& expression);
  ir::Type CheckType(const ast::Expression& expression);

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

class TypeChecker {
 public:
  TypeChecker(Context& context, Environment& environment,
              FrameAllocator& frame) noexcept
      : context_(&context), environment_(&environment), frame_(&frame) {}
  ir::Type operator()(const ast::Name&);
  ir::Type operator()(const ast::Dereference&);
  ir::Type operator()(const ast::ArrayType&);
  ir::Type operator()(const ast::SpanType&);
  ir::Type operator()(const auto& x) {
    throw Error(x.location, "value expression in type context");
  }

 private:
  ExpressionInfo CheckValue(const ast::Expression& expression);
  ir::Type CheckType(const ast::Expression& expression);

  Context* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

ir::Type CheckType(Context& context, Environment& environment,
                   FrameAllocator& frame, const ast::Expression& expression) {
  TypeChecker checker(context, environment, frame);
  return std::visit(checker, expression->value);
}

class StatementChecker {
 public:
  StatementChecker(Context& context, Environment& environment,
                   FrameAllocator& frame) noexcept
      : context_(&context), environment_(&environment), frame_(&frame) {}
  ir::Code operator()(const ast::DeclareVariable&);
  ir::Code operator()(const ast::Assign&);
  ir::Code operator()(const ast::If&);
  ir::Code operator()(const ast::While&);
  ir::Code operator()(const ast::Return&);
  ir::Code operator()(const ast::Break&);
  ir::Code operator()(const ast::Continue&);
  ir::Code operator()(const ast::DiscardedExpression&);
  ir::Code operator()(const ast::FunctionDefinition&);

 private:
  // ExpressionInfo CheckAddress(const ast::Expression& expression);
  ExpressionInfo CheckValue(const ast::Expression& expression);
  ir::Type CheckType(const ast::Expression& expression);
  ir::Code CheckBlock(Environment& parent_environment,
                      std::span<const ast::Statement> block);
  ir::Code CheckBlock(std::span<const ast::Statement> block);

  Context* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

class ModuleStatementChecker {
 public:
  ModuleStatementChecker(Context& context, Environment& environment) noexcept
      : context_(&context), environment_(&environment) {}
  ir::Code operator()(const ast::DeclareVariable&);
  ir::Code operator()(const ast::Assign&);
  ir::Code operator()(const ast::If&);
  ir::Code operator()(const ast::While&);
  ir::Code operator()(const ast::Return&);
  ir::Code operator()(const ast::Break&);
  ir::Code operator()(const ast::Continue&);
  ir::Code operator()(const ast::DiscardedExpression&);
  ir::Code operator()(const ast::FunctionDefinition&);

 private:
  // ExpressionInfo CheckAddress(const ast::Expression& expression);
  ir::Type CheckType(const ast::Expression& expression);

  Context* context_;
  Environment* environment_;
};

ExpressionInfo ExpressionChecker::operator()(const ast::Name& x) {
  auto* definition = environment_->Lookup(x.value);
  if (!definition) throw UndeclaredError(x.value, x.location);
  return ExpressionInfo{.value = AsValue(x.location, definition->value)};
}

ExpressionInfo ExpressionChecker::operator()(const ast::IntegerLiteral& x) {
  return ExpressionInfo{.value = TypedExpression(Category::kRvalue,
                                                 ir::Primitive::kInt64,
                                                 Representation::kDirect,
                                                 ir::IntegerLiteral(x.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Call& x) {
  std::vector<ir::Code> code;
  ExpressionInfo function = CheckValue(x.function);
  const ir::FunctionPointer& function_type =
      AsFunctionPointer(x.function.location(), function.value.type);
  code.push_back(std::move(function.code));
  std::vector<ir::Expression> arguments;
  // Allocate space for the function result.
  const bool has_result = ir::Size(function_type.return_type) > 0;
  const ir::Local::Offset offset = frame_->Allocate(function_type.return_type);
  if (has_result) arguments.push_back(ir::Local(offset));
  const int num_arguments = x.arguments.size();
  if (num_arguments != (int)function_type.parameters.size()) {
    throw Error(x.location, "wrong number of arguments for function of type ",
                function_type);
  }
  for (int i = 0; i < num_arguments; i++) {
    ExpressionInfo result = EnsureRvalue(
        x.arguments[i].location(), *frame_,
        ConvertTo(x.arguments[i].location(), function_type.parameters[i],
                  CheckValue(x.arguments[i])));
    code.push_back(std::move(result.code));
    arguments.push_back(std::move(result.value.value));
  }
  code.push_back(
      ir::Call(std::move(function.value.value), std::move(arguments)));
  if (has_result) {
    return ExpressionInfo{
        .code = ir::Sequence{std::move(code)},
        .value = TypedExpression(Category::kRvalue, function_type.return_type,
                                 Representation::kAddress, ir::Local(offset))};
  } else {
    return ExpressionInfo{.code = ir::Sequence{std::move(code)},
                          .value = TypedExpression(
                              Category::kRvalue, function_type.return_type,
                              Representation::kDirect, ir::IntegerLiteral(0))};
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Index& x) {
  ExpressionInfo container = CheckValue(x.container);
  ExpressionInfo index = CheckValue(x.index);
  if (index.value.type != ir::Primitive::kInt64) {
    throw Error(x.index.location(), "array index must be an integer");
  }
  if (auto* a = std::get_if<ir::Array>(&container.value.type->value)) {
    return ExpressionInfo{
        .code =
            ir::Sequence({std::move(container.code), std::move(index.code)}),
        .value = TypedExpression(
            container.value.category, a->element, Representation::kAddress,
            ir::Add(std::move(container.value.value),
                    ir::Multiply(ir::IntegerLiteral(ir::Size(a->element)),
                                 std::move(index.value.value))))};
  } else if (auto* s = std::get_if<ir::Span>(&container.value.type->value)) {
    ir::Expression address =
        container.value.representation == Representation::kDirect
            ? std::move(container.value.value)
            : ir::Load64(std::move(container.value.value));
    return ExpressionInfo{
        .code =
            ir::Sequence({std::move(container.code), std::move(index.code)}),
        .value = TypedExpression(
            Category::kLvalue, s->element, Representation::kAddress,
            ir::Add(std::move(address),
                    ir::Multiply(ir::IntegerLiteral(ir::Size(s->element)),
                                 std::move(index.value.value))))};
  } else {
    throw Error(x.container.location(), "cannot index a value of type ",
                container.value.type);
  }
}

ExpressionInfo ExpressionChecker::operator()(const ast::Negate& x) {
  ExpressionInfo inner = EnsureInt64(x.inner.location(), CheckValue(x.inner));
  return ExpressionInfo{
      .code = std::move(inner.code),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::Negate(std::move(inner.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalNot& x) {
  ExpressionInfo inner =
      EnsureComparable(x.inner.location(), CheckValue(x.inner));
  return ExpressionInfo{
      .code = std::move(inner.code),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::LogicalNot(std::move(inner.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseNot& x) {
  ExpressionInfo inner = EnsureInt64(x.inner.location(), CheckValue(x.inner));
  return ExpressionInfo{
      .code = std::move(inner.code),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::BitwiseNot(std::move(inner.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Dereference& x) {
  ExpressionInfo inner = CheckValue(x.inner);
  const auto* p = std::get_if<ir::Pointer>(&inner.value.type->value);
  if (!p) throw Error(x.location, "cannot dereference ", inner.value.type);
  ir::Expression address = inner.value.representation == Representation::kDirect
                               ? std::move(inner.value.value)
                               : ir::Load64(inner.value.value);
  return ExpressionInfo{
      .code = std::move(inner.code),
      .value = TypedExpression(Category::kLvalue, p->pointee,
                               Representation::kAddress, std::move(address))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::AddressOf& x) {
  ExpressionInfo inner = CheckValue(x.inner);
  if (inner.value.category != Category::kLvalue) {
    throw Error(x.inner.location(), "not an lvalue");
  }
  return ExpressionInfo{
      .code = std::move(inner.code),
      .value = TypedExpression{
          Category::kRvalue, ir::Pointer(std::move(inner.value.type)),
          Representation::kDirect, std::move(inner.value.value)}};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Add& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(
          Category::kRvalue, std::move(left.value.type),
          Representation::kDirect,
          ir::Add(std::move(left.value.value), std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Subtract& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
                               Representation::kDirect,
                               ir::Subtract(std::move(left.value.value),
                                            std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Multiply& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
                               Representation::kDirect,
                               ir::Multiply(std::move(left.value.value),
                                            std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Divide& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
                               Representation::kDirect,
                               ir::Divide(std::move(left.value.value),
                                          std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Modulo& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
                               Representation::kDirect,
                               ir::Modulo(std::move(left.value.value),
                                          std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::As& x) {
  ExpressionInfo value = CheckValue(x.value);
  ir::Type type = CheckType(x.type);
  if (value.value.type == type) return value;
  // Conversion from int64 to byte
  if (value.value.type == ir::Primitive::kInt64 &&
      type == ir::Primitive::kByte) {
    value.value.type = std::move(type);
    return value;
  }
  // Conversion to or from *void.
  {
    const auto* from = std::get_if<ir::Pointer>(&value.value.type->value);
    const auto* to = std::get_if<ir::Pointer>(&type->value);
    if (from && to &&
        (from->pointee == ir::Primitive::kVoid ||
         to->pointee == ir::Primitive::kVoid)) {
      value.value.type = type;
      return value;
    }
  }
  throw Error(x.value.location(), "cannot cast ", value.value.type, " to ",
              type);
}

ExpressionInfo ExpressionChecker::operator()(const ast::LessThan& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
          Representation::kDirect,
                               ir::LessThan(std::move(left.value.value),
                                            std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LessOrEqual& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
                               Representation::kDirect,
                               ir::LessOrEqual(std::move(left.value.value),
                                               std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::GreaterThan& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  // GreaterThan(left, right) is translated into LessThan(right, left).
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
                               Representation::kDirect,
                               ir::LessThan(std::move(right.value.value),
                                            std::move(left.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::GreaterOrEqual& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  // GreaterOrEqual(left, right) is translated into LessOrEqual(right, left).
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, std::move(left.value.type),
                               Representation::kDirect,
                               ir::LessOrEqual(std::move(right.value.value),
                                               std::move(left.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Equal& x) {
  ExpressionInfo left = EnsureComparable(x.left.location(), CheckValue(x.left));
  ExpressionInfo right =
      EnsureComparable(x.right.location(), CheckValue(x.right));
  if (left.value.type != right.value.type) {
    throw Error(x.location, "incompatible types for equality comparison: ",
                left.value.type, " and ", right.value.type);
  }
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::Equal(std::move(left.value.value),
                                         std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::NotEqual& x) {
  ExpressionInfo left = EnsureComparable(x.left.location(), CheckValue(x.left));
  ExpressionInfo right =
      EnsureComparable(x.right.location(), CheckValue(x.right));
  if (left.value.type != right.value.type) {
    throw Error(x.location, "incompatible types for equality comparison: ",
                left.value.type, " and ", right.value.type);
  }
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::NotEqual(std::move(left.value.value),
                                            std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalAnd& x) {
  ExpressionInfo left = EnsureComparable(x.left.location(), CheckValue(x.left));
  ExpressionInfo right =
      EnsureComparable(x.right.location(), CheckValue(x.right));
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(ir::Primitive::kInt64);
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
           ir::Store64(ir::Local(offset), std::move(left.value.value)),
           ir::JumpUnless(ir::Load64(ir::Local(offset)), end),
           std::move(right.code),
           ir::Store64(ir::Local(offset), std::move(right.value.value)), end}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kAddress, ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalOr& x) {
  ExpressionInfo left = EnsureComparable(x.left.location(), CheckValue(x.left));
  ExpressionInfo right =
      EnsureComparable(x.right.location(), CheckValue(x.right));
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(ir::Primitive::kInt64);
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
           ir::Store64(ir::Local(offset), std::move(left.value.value)),
           ir::JumpIf(ir::Load64(ir::Local(offset)), end),
           std::move(right.code),
           ir::Store64(ir::Local(offset), std::move(right.value.value)), end}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kAddress, ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseAnd& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::BitwiseAnd(std::move(left.value.value),
                                              std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseOr& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::BitwiseOr(std::move(left.value.value),
                                             std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseXor& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::BitwiseXor(std::move(left.value.value),
                                              std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftLeft& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::ShiftLeft(std::move(left.value.value),
                                             std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftRight& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Primitive::kInt64,
                               Representation::kDirect,
                               ir::ShiftRight(std::move(left.value.value),
                                              std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::ArrayType& x) {
  throw Error(x.location, "type expression in value context");
}

ExpressionInfo ExpressionChecker::operator()(const ast::SpanType& x) {
  throw Error(x.location, "type expression in value context");
}

ExpressionInfo ExpressionChecker::operator()(const ast::TernaryExpression& x) {
  ExpressionInfo condition =
      EnsureComparable(x.condition.location(), CheckValue(x.condition));
  // TODO: Improve temporary allocation to allow using the same space for the
  // then branch and the else branch.
  ExpressionInfo then_branch = EnsureRvalue(x.then_branch.location(), *frame_,
                                            CheckValue(x.then_branch));
  ExpressionInfo else_branch = EnsureRvalue(x.else_branch.location(), *frame_,
                                            CheckValue(x.else_branch));
  if (then_branch.value.representation != Representation::kAddress ||
      else_branch.value.representation != Representation::kAddress) {
    // We need addresses for each value for the implementation below.
    std::abort();
  }
  if (then_branch.value.type != else_branch.value.type) {
    throw Error(x.location,
                "ternary expression branches yield different types: ",
                then_branch.value.type, " and ", else_branch.value.type);
  }
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(then_branch.value.type);
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
           ir::JumpUnless(std::move(condition.value.value), if_false),
           std::move(then_branch.code),
           ir::Store64(ir::Local(offset), std::move(then_branch.value.value)),
           ir::Jump(end), if_false, std::move(else_branch.code),
           ir::Store64(ir::Local(offset), std::move(else_branch.value.value)),
           end}),
      .value = TypedExpression(
          Category::kRvalue, std::move(then_branch.value.type),
          // EnsureRvalue gives us address representations and the
          // expression ensures that we put this address in the output slot.
          Representation::kAddress, ir::Load64(ir::Local(offset)))};
}

ExpressionInfo ExpressionChecker::CheckValue(const ast::Expression& x) {
  return aoc2021::CheckValue(*context_, *environment_, *frame_, x);
}

ir::Type ExpressionChecker::CheckType(const ast::Expression& x) {
  return aoc2021::CheckType(*context_, *environment_, *frame_, x);
}

ir::Type TypeChecker::operator()(const ast::Name& x) {
  auto* definition = environment_->Lookup(x.value);
  if (!definition) throw UndeclaredError(x.value, x.location);
  return AsType(x.location, definition->value);
}

ir::Type TypeChecker::operator()(const ast::Dereference& x) {
  return ir::Pointer(CheckType(x.inner));
}

ir::Type TypeChecker::operator()(const ast::ArrayType& x) {
  auto size =
      Evaluate(EnsureInt64(x.size.location(), CheckValue(x.size)).value.value);
  if (!size) {
    throw Error(x.size.location(), "array size must be a constant expression");
  }
  return ir::Array(*size, CheckType(x.element_type));
}

ir::Type TypeChecker::operator()(const ast::SpanType& x) {
  return ir::Span(CheckType(x.element_type));
}

ExpressionInfo TypeChecker::CheckValue(const ast::Expression& x) {
  return aoc2021::CheckValue(*context_, *environment_, *frame_, x);
}

ir::Type TypeChecker::CheckType(const ast::Expression& x) {
  return aoc2021::CheckType(*context_, *environment_, *frame_, x);
}

ir::Code CheckBlock(Context& context, Environment& parent_environment,
                    FrameAllocator& parent_frame,
                    std::span<const ast::Statement> block) {
  std::vector<ir::Code> code;
  FrameAllocator frame(&parent_frame);
  Environment environment(parent_environment, Environment::ShadowMode::kDeny);
  for (const auto& statement : block) {
    StatementChecker checker(context, environment, frame);
    code.push_back(std::visit(checker, statement->value));
  }
  return ir::Sequence(std::move(code));
}

ir::Code StatementChecker::operator()(const ast::DeclareVariable& x) {
  ir::Type type = CheckType(x.type);
  const ir::Local::Offset offset = frame_->Allocate(type);
  environment_->Define(
      x.name, Environment::Definition{
                  .location = x.location,
                  .value = TypedExpression(Category::kLvalue, std::move(type),
                                           Representation::kAddress,
                                           ir::Local(offset))});
  return ir::Sequence();
}

ir::Code StatementChecker::operator()(const ast::Assign& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  if (left.value.category != Category::kLvalue) {
    throw Error(x.left.location(), "not an lvalue");
  }
  right = ConvertTo(x.right.location(), left.value.type, std::move(right));
  return ir::Sequence(
      {std::move(left.code), std::move(right.code),
       ir::Store64(std::move(left.value.value), std::move(right.value.value))});
}

ir::Code StatementChecker::operator()(const ast::If& x) {
  ExpressionInfo condition =
      EnsureComparable(x.condition.location(), CheckValue(x.condition));
  ir::Code then_branch = CheckBlock(x.then_branch);
  ir::Code else_branch = CheckBlock(x.else_branch);
  const ir::Label if_false = context_->Label("if_false");
  const ir::Label end = context_->Label("if_end");
  return ir::Sequence(
      {std::move(condition.code),
       ir::JumpUnless(std::move(condition.value.value), if_false),
       std::move(then_branch), ir::Jump(end), if_false, std::move(else_branch),
       end});
}

ir::Code StatementChecker::operator()(const ast::While& x) {
  const ir::Label loop_start = context_->Label("while_start");
  const ir::Label loop_condition = context_->Label("while_condition");
  const ir::Label loop_end = context_->Label("while_end");
  ExpressionInfo condition = CheckValue(x.condition);
  Environment while_environment(*environment_, Environment::ShadowMode::kDeny);
  while_environment.SetBreak(loop_end);
  while_environment.SetContinue(loop_condition);
  ir::Code body = CheckBlock(while_environment, x.body);
  return ir::Sequence({ir::Jump(loop_condition), loop_start, std::move(body),
                       loop_condition, std::move(condition.code),
                       ir::JumpIf(std::move(condition.value.value), loop_start),
                       loop_end});
}

ir::Code StatementChecker::operator()(const ast::Return& x) {
  // TODO: Check that the return type matches the return type of the function.
  if (x.value) {
    ExpressionInfo value = CheckValue(*x.value);
    return ir::Sequence({std::move(value.code),
                         ir::Store64(ir::Local(ir::Local::Offset(8)),
                                     std::move(value.value.value)),
                         ir::Return()});
  } else {
    return ir::Return();
  }
}

ir::Code StatementChecker::operator()(const ast::Break& x) {
  auto label = environment_->Break();
  if (!label) {
    throw Error(x.location, "break statement outside of a breakable context");
  }
  return ir::Jump(*label);
}

ir::Code StatementChecker::operator()(const ast::Continue& x) {
  auto label = environment_->Break();
  if (!label) {
    throw Error(x.location, "continue statement outside of a loop");
  }
  return ir::Jump(*label);
}

ir::Code StatementChecker::operator()(const ast::DiscardedExpression& x) {
  return CheckValue(x.expression).code;
}

ir::Code StatementChecker::operator()(const ast::FunctionDefinition& x) {
  throw Error(x.location, "nested function definitions are forbidden");
}

ExpressionInfo StatementChecker::CheckValue(const ast::Expression& x) {
  return aoc2021::CheckValue(*context_, *environment_, *frame_, x);
}

ir::Type StatementChecker::CheckType(const ast::Expression& x) {
  return aoc2021::CheckType(*context_, *environment_, *frame_, x);
}

ir::Code StatementChecker::CheckBlock(
    Environment& parent_environment, std::span<const ast::Statement> block) {
  return aoc2021::CheckBlock(*context_, parent_environment, *frame_, block);
}

ir::Code StatementChecker::CheckBlock(
    std::span<const ast::Statement> block) {
  return CheckBlock(*environment_, block);
}

ir::Code ModuleStatementChecker::operator()(const ast::DeclareVariable& x) {
  ir::Type type = CheckType(x.type);
  const ir::Global global = context_->Global(x.name, ir::Size(type));
  environment_->Define(
      x.name, Environment::Definition{
                  .location = x.location,
                  .value = TypedExpression(Category::kLvalue, std::move(type),
                                           Representation::kAddress, global)});
  return ir::Sequence();
}

ir::Code ModuleStatementChecker::operator()(const ast::Assign& x) {
  throw Error(x.location,
              "assignment statements are forbidden at module scope");
}

ir::Code ModuleStatementChecker::operator()(const ast::If& x) {
  throw Error(x.location,
              "if statements are forbidden at module scope");
}

ir::Code ModuleStatementChecker::operator()(const ast::While& x) {
  throw Error(x.location,
              "while statements are forbidden at module scope");
}

ir::Code ModuleStatementChecker::operator()(const ast::Return& x) {
  throw Error(x.location, "return statement outside of a function");
}

ir::Code ModuleStatementChecker::operator()(const ast::Break& x) {
  throw Error(x.location, "break statement outside of a breakable context");
}

ir::Code ModuleStatementChecker::operator()(const ast::Continue& x) {
  throw Error(x.location, "continue statement outside of a loop");
}

ir::Code ModuleStatementChecker::operator()(
    const ast::DiscardedExpression& x) {
  throw Error(x.location,
              "discarded expressions are forbidden at module scope");
}

ir::Code ModuleStatementChecker::operator()(const ast::FunctionDefinition& x) {
  const ir::Label function = context_->Label("function");
  ir::Type return_type = CheckType(x.return_type);
  std::vector<ir::Type> parameters;
  for (const auto& [name, type] : x.parameters) {
    parameters.push_back(CheckType(type));
  }
  // TODO: Derive symbolic constant names in a better way.
  environment_->Define(
      x.name,
      Environment::Definition{
          .location = x.location,
          .value = TypedExpression(Category::kRvalue,
                                   ir::FunctionPointer(return_type, parameters),
                                   Representation::kDirect, function)});
  if (x.name == "main") context_->SetMain(function);
  Environment function_environment(*environment_,
                                   Environment::ShadowMode::kAllow);
  const int n = x.parameters.size();
  // Parameters are arranged above the function stack frame:
  //  ...
  //  arg2
  //  arg1
  //  [return slot] <- omitted for zero-sized return types.
  //  return address
  //  saved frame pointer <- frame pointer points here
  //  ...
  //  local2
  //  local1
  const int args_begin = ir::Size(return_type) == 0 ? 2 : 3;
  for (int i = 0; i < n; i++) {
    const auto& parameter = x.parameters[i];
    const ir::Local::Offset offset{8 * (i + args_begin)};
    function_environment.Define(
        parameter.name.value,
        Environment::Definition{
            .location = parameter.name.location,
            .value =
                TypedExpression(Category::kLvalue, parameters[i],
                                Representation::kAddress, ir::Local(offset))});
  }

  FrameAllocator frame;
  ir::Code code = CheckBlock(*context_, function_environment, frame, x.body);
  return ir::Sequence({function, ir::BeginFrame(frame.max_size()),
                       std::move(code), ir::Return()});
}

ir::Type ModuleStatementChecker::CheckType(const ast::Expression& x) {
  // All expressions at global scope must be constant expressions, so the frame
  // is not actually needed at runtime.
  FrameAllocator frame;
  return aoc2021::CheckType(*context_, *environment_, frame, x);
}

Location BuiltinLocation() {
  const Source& instance = *new Source("builtin", "");
  Reader reader(instance);
  return reader.location();
}

}  // namespace

ir::Unit Check(std::span<const ast::Statement> program) {
  Context context;
  Environment global;
  global.Define("void",
                Environment::Definition{.location = BuiltinLocation(),
                                        .value = ir::Primitive::kVoid});
  global.Define("byte", Environment::Definition{.location = BuiltinLocation(),
                                                .value = ir::Primitive::kByte});
  global.Define("int64",
                Environment::Definition{.location = BuiltinLocation(),
                                        .value = ir::Primitive::kInt64});
  global.Define("read",
                Environment::Definition{
                    .location = BuiltinLocation(),
                    .value = TypedExpression(
                        Category::kRvalue,
                        ir::FunctionPointer(ir::Primitive::kInt64,
                                            {ir::Primitive::kInt64,
                                             ir::Pointer(ir::Primitive::kVoid),
                                             ir::Primitive::kInt64}),
                        Representation::kDirect, ir::Label("read"))});
  global.Define("write",
                Environment::Definition{
                    .location = BuiltinLocation(),
                    .value = TypedExpression(
                        Category::kRvalue,
                        ir::FunctionPointer(ir::Primitive::kInt64,
                                            {ir::Primitive::kInt64,
                                             ir::Pointer(ir::Primitive::kVoid),
                                             ir::Primitive::kInt64}),
                        Representation::kDirect, ir::Label("write"))});
  global.Define("exit", Environment::Definition{
                            .location = BuiltinLocation(),
                            .value = TypedExpression(
                                Category::kRvalue,
                                ir::FunctionPointer(ir::Primitive::kInt64,
                                                    {ir::Primitive::kInt64}),
                                Representation::kDirect, ir::Label("exit"))});
  std::vector<ir::Code> code;
  for (const auto& statement : program) {
    ModuleStatementChecker checker(context, global);
    code.push_back(std::visit(checker, statement->value));
  }
  return ir::Unit{.main = context.Main(),
                  .data = context.Globals(),
                  .code = ir::Flatten(ir::Sequence(std::move(code)))};
}

}  // namespace aoc2021
