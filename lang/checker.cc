#include "checker.h"

#include <cassert>
#include <map>
#include <variant>

#include "parser.h"
#include "string_utils.h"

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
  std::optional<std::int64_t> operator()(const ir::Load8& x) {
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

Location BuiltinLocation() {
  const Source& instance = *new Source("builtin", "");
  Reader reader(instance);
  return reader.location();
}

void AddBuiltins(Environment& environment) {
  environment.Define("void",
                     Environment::Definition{.location = BuiltinLocation(),
                                             .value = ir::Void{}});
  environment.Define("byte",
                     Environment::Definition{.location = BuiltinLocation(),
                                             .value = ir::Scalar::kByte});
  environment.Define("int64",
                     Environment::Definition{.location = BuiltinLocation(),
                                             .value = ir::Scalar::kInt64});
  environment.Define(
      "read",
      Environment::Definition{
          .location = BuiltinLocation(),
          .value = TypedExpression(
              Category::kRvalue,
              ir::FunctionPointer(ir::Scalar::kInt64,
                                  {ir::Scalar::kInt64, ir::Pointer(ir::Void{}),
                                   ir::Scalar::kInt64}),
              Representation::kDirect, ir::Label("read"))});
  environment.Define(
      "write",
      Environment::Definition{
          .location = BuiltinLocation(),
          .value = TypedExpression(
              Category::kRvalue,
              ir::FunctionPointer(ir::Scalar::kInt64,
                                  {ir::Scalar::kInt64, ir::Pointer(ir::Void{}),
                                   ir::Scalar::kInt64}),
              Representation::kDirect, ir::Label("write"))});
  environment.Define(
      "exit", Environment::Definition{
                  .location = BuiltinLocation(),
                  .value = TypedExpression(
                      Category::kRvalue,
                      ir::FunctionPointer(ir::Void{}, {ir::Scalar::kInt64}),
                      Representation::kDirect, ir::Label("exit"))});
}

Environment CreateBuiltinEnvironment() {
  Environment environment;
  AddBuiltins(environment);
  return environment;
}

Environment& BuiltinEnvironment() {
  static Environment& instance = *new Environment(CreateBuiltinEnvironment());
  return instance;
}

class ModuleContext {
 public:
  ModuleContext(Checker& checker, Checker::Entry& entry) noexcept
      : checker_(&checker),
        entry_(&entry),
        unexported_(entry.exports, Environment::ShadowMode::kDeny) {}

  ir::Label Label(std::string_view prefix) {
    return checker_->NextLabel(prefix);
  }

  ir::Global Global(std::string_view prefix, std::int64_t size) {
    ir::Global result = checker_->NextGlobal(prefix);
    // TODO: Handle alignment.
    globals_.emplace(result, size);
    return result;
  }

  ir::Global StringLiteral(std::string_view value) {
    ir::Global result = checker_->NextGlobal("string_literal");
    string_literals_.emplace(result, value);
    return result;
  }

  ir::Struct::Id Struct() { return checker_->NextStruct(); }

  void SetMain(ir::Label label) { main_ = std::move(label); }
  const std::optional<ir::Label>& Main() const { return main_; }

  const std::map<ir::Global, std::int64_t>& Globals() const { return globals_; }

  const std::map<ir::Global, std::string>& StringLiterals() const {
    return string_literals_;
  }

  Environment& ExportedEnvironment() { return entry_->exports; }
  Environment& UnexportedEnvironment() { return unexported_; }

  const Environment& EnvironmentFor(const std::filesystem::path& path) const {
    return checker_->EnvironmentFor(path);
  }

  const std::filesystem::path& Path() const { return entry_->path; }

 private:
  Checker* checker_;
  Checker::Entry* entry_;
  Environment unexported_;

  std::optional<ir::Label> main_;
  std::map<ir::Global, std::int64_t> globals_;
  std::map<ir::Global, std::string> string_literals_;
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

TypedExpression EnsureLoaded(Location location, TypedExpression input) {
  switch (ir::Size(input.type)) {
    case 0:
      return TypedExpression{.category = Category::kRvalue,
                             .type = std::move(input.type),
                             .representation = Representation::kDirect,
                             .value = ir::IntegerLiteral(0)};
    case 1:
      if (input.representation == Representation::kDirect) return input;
      return TypedExpression{.category = Category::kRvalue,
                             .type = std::move(input.type),
                             .representation = Representation::kDirect,
                             .value = ir::Load8(std::move(input.value))};
    case 8:
      if (input.representation == Representation::kDirect) return input;
      return TypedExpression{.category = Category::kRvalue,
                             .type = std::move(input.type),
                             .representation = Representation::kDirect,
                             .value = ir::Load64(std::move(input.value))};
    default:
      throw Error(location, "cannot load value of type ", input.type);
  }
}

ExpressionInfo EnsureRvalue(Location location, FrameAllocator& frame,
                            ExpressionInfo info) {
  if (info.value.category == Category::kRvalue) return info;
  const ir::Local::Offset copy = frame.Allocate(info.value.type);
  if (info.value.representation != Representation::kAddress) std::abort();
  return ExpressionInfo{
      .code = ir::Sequence(
          {std::move(info.code),
           ir::Call(ir::Label("copy"),
                    {ir::Local(copy), info.value.value,
                     ir::IntegerLiteral(ir::Size(info.value.type))})}),
      .value = TypedExpression{.category = Category::kRvalue,
                               .type = info.value.type,
                               .representation = Representation::kAddress,
                               .value = ir::Local(copy)}};
}

ExpressionInfo EnsureInt64(Location location, ExpressionInfo info) {
  if (info.value.type != ir::Scalar::kInt64 &&
      info.value.type != ir::Scalar::kByte) {
    throw Error(location, "not an integer");
  }
  info.value = EnsureLoaded(location, info.value);
  info.value.type = ir::Scalar::kInt64;
  return info;
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
        .value = EnsureLoaded(location, std::move(info.value))};
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

ir::Code DoStore(Location location, ir::Expression address, ir::Type type,
                 ExpressionInfo info) {
  info = ConvertTo(location, type, std::move(info));
  switch (ir::Size(type)) {
    case 0:
      return ir::Sequence();
    case 1:
      return ir::Store8(std::move(address),
                        EnsureLoaded(location, std::move(info.value)).value);
    case 8:
      return ir::Store64(std::move(address),
                         EnsureLoaded(location, std::move(info.value)).value);
    default:
      return ir::Call(ir::Label("copy"),
                      {std::move(address), std::move(info.value.value),
                       ir::IntegerLiteral(ir::Size(type))});
  }
}

// Returns true if the given type should be represented directly by a parameter.
// If true, the parameter slot directly holds the object.
// If false, the parameter slot holds a pointer to the object.
bool DirectParameter(const ir::Type& type) {
  const std::int64_t size = ir::Size(type);
  // TODO: Allow objects of any size [0..8] to be passed directly.
  return size == 0 || size == 1 || size == 8;
}

ExpressionInfo PrepareParameter(Location location, FrameAllocator& frame,
                                ExpressionInfo info) {
  if (DirectParameter(info.value.type)) {
    return ExpressionInfo{
        .code = std::move(info.code),
        .value = EnsureLoaded(location, std::move(info.value))};
  } else if (info.value.category == Category::kRvalue &&
             info.value.representation == Representation::kAddress) {
    return info;
  } else {
    const ir::Local::Offset copy = frame.Allocate(info.value.type);
    return ExpressionInfo{
        .code = ir::Sequence(
            {std::move(info.code),
             ir::Call(ir::Label("copy"),
                      {ir::Local(copy), std::move(info.value.value),
                       ir::IntegerLiteral(ir::Size(info.value.type))})}),
        .value = TypedExpression{.category = Category::kRvalue,
                                 .type = std::move(info.value.type),
                                 .representation = Representation::kAddress,
                                 .value = std::move(ir::Local(copy))}};
  }
}

TypedExpression AccessParameter(Location location, ir::Type type,
                                std::int64_t offset) {
  if (DirectParameter(type)) {
    return TypedExpression{.category = Category::kLvalue,
                           .type = std::move(type),
                           .representation = Representation::kAddress,
                           .value = ir::Local(ir::Local::Offset{offset})};
  } else {
    return TypedExpression{
        .category = Category::kLvalue,
        .type = std::move(type),
        .representation = Representation::kAddress,
        .value = ir::Load64(ir::Local(ir::Local::Offset{offset}))};
  }
}

const ir::FunctionPointer& AsFunctionPointer(Location location,
                                             const ir::FunctionPointer& x) {
  return x;
}

const ir::FunctionPointer& AsFunctionPointer(Location location, const auto& x) {
  throw Error(location, "trying to use ", x, " as a function");
}

const ir::FunctionPointer& AsFunctionPointer(Location location,
                                             const ir::Type& x) {
  return std::visit(
      [&](const auto& x) -> const ir::FunctionPointer& {
        return AsFunctionPointer(location, x);
      },
      x->value);
}

class ExpressionChecker {
 public:
  ExpressionChecker(ModuleContext& context, Environment& environment,
                    FrameAllocator& frame) noexcept
      : context_(&context), environment_(&environment), frame_(&frame) {}
  ExpressionInfo operator()(const ast::Name&);
  ExpressionInfo operator()(const ast::CharacterLiteral&);
  ExpressionInfo operator()(const ast::IntegerLiteral&);
  ExpressionInfo operator()(const ast::StringLiteral&);
  ExpressionInfo operator()(const ast::Access&);
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

  ModuleContext* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

ExpressionInfo CheckValue(ModuleContext& context, Environment& environment,
                          FrameAllocator& frame,
                          const ast::Expression& expression) {
  ExpressionChecker checker(context, environment, frame);
  return std::visit(checker, expression->value);
}

class TypeChecker {
 public:
  TypeChecker(ModuleContext& context, Environment& environment,
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

  ModuleContext* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

ir::Type CheckType(ModuleContext& context, Environment& environment,
                   FrameAllocator& frame, const ast::Expression& expression) {
  TypeChecker checker(context, environment, frame);
  return std::visit(checker, expression->value);
}

class StatementChecker {
 public:
  StatementChecker(ModuleContext& context, Environment& environment,
                   FrameAllocator& frame) noexcept
      : context_(&context), environment_(&environment), frame_(&frame) {}
  ir::Code operator()(const ast::Import&);
  ir::Code operator()(const ast::Export&);
  ir::Code operator()(const ast::DeclareVariable&);
  ir::Code operator()(const ast::Assign&);
  ir::Code operator()(const ast::DeclareAndAssign&);
  ir::Code operator()(const ast::If&);
  ir::Code operator()(const ast::While&);
  ir::Code operator()(const ast::Return&);
  ir::Code operator()(const ast::Break&);
  ir::Code operator()(const ast::Continue&);
  ir::Code operator()(const ast::DiscardedExpression&);
  ir::Code operator()(const ast::FunctionDefinition&);
  ir::Code operator()(const ast::StructDefinition&);

 private:
  // ExpressionInfo CheckAddress(const ast::Expression& expression);
  ExpressionInfo CheckValue(const ast::Expression& expression);
  ir::Type CheckType(const ast::Expression& expression);
  ir::Code CheckBlock(Environment& parent_environment,
                      std::span<const ast::Statement> block);
  ir::Code CheckBlock(std::span<const ast::Statement> block);

  ModuleContext* context_;
  Environment* environment_;
  FrameAllocator* frame_;
};

class ModuleStatementChecker {
 public:
  ModuleStatementChecker(
      ModuleContext& context, Environment& environment) noexcept
      : context_(&context), environment_(&environment) {}
  ir::Code operator()(const ast::Import&);
  ir::Code operator()(const ast::Export&);
  ir::Code operator()(const ast::DeclareVariable&);
  ir::Code operator()(const ast::Assign&);
  ir::Code operator()(const ast::DeclareAndAssign&);
  ir::Code operator()(const ast::If&);
  ir::Code operator()(const ast::While&);
  ir::Code operator()(const ast::Return&);
  ir::Code operator()(const ast::Break&);
  ir::Code operator()(const ast::Continue&);
  ir::Code operator()(const ast::DiscardedExpression&);
  ir::Code operator()(const ast::FunctionDefinition&);
  ir::Code operator()(const ast::StructDefinition&);

 private:
  // ExpressionInfo CheckAddress(const ast::Expression& expression);
  ir::Type CheckType(const ast::Expression& expression);

  ModuleContext* context_;
  Environment* environment_;
};

ExpressionInfo ExpressionChecker::operator()(const ast::Name& x) {
  auto* definition = environment_->Lookup(x.value);
  if (!definition) throw UndeclaredError(x.value, x.location);
  return ExpressionInfo{.value = AsValue(x.location, definition->value)};
}

ExpressionInfo ExpressionChecker::operator()(const ast::CharacterLiteral& x) {
  return ExpressionInfo{.value = TypedExpression(Category::kRvalue,
                                                 ir::Scalar::kByte,
                                                 Representation::kDirect,
                                                 ir::IntegerLiteral(x.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::IntegerLiteral& x) {
  return ExpressionInfo{.value = TypedExpression(Category::kRvalue,
                                                 ir::Scalar::kInt64,
                                                 Representation::kDirect,
                                                 ir::IntegerLiteral(x.value))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::StringLiteral& x) {
  const ir::Global address = context_->StringLiteral(x.value);
  return ExpressionInfo{
      .value = TypedExpression(
          Category::kRvalue,
          // A string literal is an array of bytes including a null-terminator.
          ir::Pointer(ir::Array(x.value.size() + 1, ir::Scalar::kByte)),
          Representation::kDirect, address)};
}

ExpressionInfo ExpressionChecker::operator()(const ast::Access& x) {
  ExpressionInfo object = CheckValue(x.object);
  if (auto* m = std::get_if<ir::Module>(&object.value.type->value)) {
    auto* definition = context_->EnvironmentFor(m->path).Lookup(x.field.value);
    if (!definition) throw UndeclaredError(x.field.value, x.location);
    return ExpressionInfo{.value = AsValue(x.location, definition->value)};
  } else if (auto* s = std::get_if<ir::Struct>(&object.value.type->value)) {
    auto i = s->fields.find(x.field.value);
    if (i == s->fields.end()) {
      throw Error(x.location, "no such field ", x.field.value, " in ",
                  object.value.type);
    }
    if (object.value.representation != Representation::kAddress) {
      throw Error(x.location,
                  "member access for directly-represented objects is not "
                  "implemented in the compiler");
    }
    return ExpressionInfo{
        .code = std::move(object.code),
        .value = TypedExpression{
            .category = object.value.category,
            .type = i->second.type,
            .representation = Representation::kAddress,
            .value = ir::Add(std::move(object.value.value),
                             ir::IntegerLiteral(i->second.offset))}};
  } else {
    throw Error(x.location, "cannot perform member access for ",
                object.value.type);
  }
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
    ExpressionInfo result = PrepareParameter(
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
  ExpressionInfo index = EnsureInt64(x.index.location(), CheckValue(x.index));
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
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::Negate(std::move(inner.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalNot& x) {
  ExpressionInfo inner =
      EnsureComparable(x.inner.location(), CheckValue(x.inner));
  return ExpressionInfo{
      .code = std::move(inner.code),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::LogicalNot(std::move(inner.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseNot& x) {
  ExpressionInfo inner = EnsureInt64(x.inner.location(), CheckValue(x.inner));
  return ExpressionInfo{
      .code = std::move(inner.code),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
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

struct CheckAdd {
  CheckAdd(const ast::Add& x, ExpressionInfo& left, ExpressionInfo& right)
      : x(x), left(left), right(right) {}

  // Promote two scalars to int64 and perform arithmetic.
  TypedExpression operator()(const ir::Scalar&, const ir::Scalar&) {
    return TypedExpression(
        Category::kRvalue, ir::Scalar::kInt64, Representation::kDirect,
        ir::Add(
            EnsureLoaded(x.left.location(), std::move(left.value)).value,
            EnsureLoaded(x.right.location(), std::move(right.value)).value));
  }

  // Pointer arithmetic on a span.
  TypedExpression operator()(const ir::Span& s, const ir::Scalar&) {
    ir::Expression address =
        left.value.representation == Representation::kDirect
            ? std::move(left.value.value)
            : ir::Load64(std::move(left.value.value));
    TypedExpression index =
        EnsureLoaded(x.right.location(), std::move(right.value));
    return TypedExpression(
        Category::kLvalue, left.value.type, Representation::kDirect,
        ir::Add(std::move(address),
                ir::Multiply(ir::IntegerLiteral(ir::Size(s.element)),
                             std::move(index).value)));
  }

  // Pointer arithmetic on a span.
  TypedExpression operator()(const ir::Scalar&, const ir::Span& s) {
    ir::Expression address =
        right.value.representation == Representation::kDirect
            ? std::move(right.value.value)
            : ir::Load64(std::move(right.value.value));
    TypedExpression index =
        EnsureLoaded(x.left.location(), std::move(left.value));
    return TypedExpression(
        Category::kLvalue, right.value.type, Representation::kDirect,
        ir::Add(std::move(address),
                ir::Multiply(ir::IntegerLiteral(ir::Size(s.element)),
                             std::move(index).value)));
  }

  // Pointer arithmetic by implicit conversion to a span.
  TypedExpression operator()(const ir::Pointer& p, const ir::Scalar& s) {
    if (auto* a = std::get_if<ir::Array>(&p.pointee->value)) {
      return (*this)(ir::Span(a->element), s);
    } else {
      InvalidAdd();
    }
  }

  // Pointer arithmetic by implicit conversion to a span.
  TypedExpression operator()(const ir::Scalar& s, const ir::Pointer& p) {
    if (auto* a = std::get_if<ir::Array>(&p.pointee->value)) {
      return (*this)(s, ir::Span(a->element));
    } else {
      InvalidAdd();
    }
  }

  TypedExpression operator()(const auto&, const auto&) { InvalidAdd(); }

  [[noreturn]] void InvalidAdd() {
    throw Error(x.location, "cannot add ", left.value.type, " and ",
                right.value.type);
  }

  const ast::Add& x;
  ExpressionInfo& left;
  ExpressionInfo& right;
};

ExpressionInfo ExpressionChecker::operator()(const ast::Add& x) {
  ExpressionInfo left = CheckValue(x.left);
  ExpressionInfo right = CheckValue(x.right);
  TypedExpression value =
      std::visit(CheckAdd(x, left, right), left.value.type->value,
                 right.value.type->value);
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = std::move(value)};
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
  if (value.value.type == ir::Scalar::kInt64 && type == ir::Scalar::kByte) {
    value.value.type = std::move(type);
    return value;
  }
  // Conversion from *T or []T to *void.
  if (const auto* to = std::get_if<ir::Pointer>(&type->value);
      to && to->pointee == ir::Void{}) {
    if (std::get_if<ir::Pointer>(&value.value.type->value) ||
        std::get_if<ir::Span>(&value.value.type->value)) {
      value.value.type = std::move(type);
      return value;
    }
  }
  // Conversion from *void to *T or []T.
  if (const auto* from = std::get_if<ir::Pointer>(&value.value.type->value);
      from && from->pointee == ir::Void{}) {
    if (std::get_if<ir::Pointer>(&type->value) ||
        std::get_if<ir::Span>(&type->value)) {
      value.value.type = std::move(type);
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
    throw Error(x.location,
                "incompatible types for equality comparison: ", left.value.type,
                " and ", right.value.type);
  }
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::Equal(std::move(left.value.value),
                                         std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::NotEqual& x) {
  ExpressionInfo left = EnsureComparable(x.left.location(), CheckValue(x.left));
  ExpressionInfo right =
      EnsureComparable(x.right.location(), CheckValue(x.right));
  if (left.value.type != right.value.type) {
    throw Error(x.location,
                "incompatible types for equality comparison: ", left.value.type,
                " and ", right.value.type);
  }
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::NotEqual(std::move(left.value.value),
                                            std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalAnd& x) {
  ExpressionInfo left = EnsureComparable(x.left.location(), CheckValue(x.left));
  ExpressionInfo right =
      EnsureComparable(x.right.location(), CheckValue(x.right));
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(ir::Scalar::kInt64);
  const ir::Label end = context_->Label(".Llogical_end");
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
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kAddress, ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::LogicalOr& x) {
  ExpressionInfo left = EnsureComparable(x.left.location(), CheckValue(x.left));
  ExpressionInfo right =
      EnsureComparable(x.right.location(), CheckValue(x.right));
  // Allocate space for the function result.
  const ir::Local::Offset offset = frame_->Allocate(ir::Scalar::kInt64);
  const ir::Label end = context_->Label(".Llogical_or_end");
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
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kAddress, ir::Local(offset))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseAnd& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::BitwiseAnd(std::move(left.value.value),
                                              std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseOr& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::BitwiseOr(std::move(left.value.value),
                                             std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::BitwiseXor& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::BitwiseXor(std::move(left.value.value),
                                              std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftLeft& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
                               Representation::kDirect,
                               ir::ShiftLeft(std::move(left.value.value),
                                             std::move(right.value.value)))};
}

ExpressionInfo ExpressionChecker::operator()(const ast::ShiftRight& x) {
  ExpressionInfo left = EnsureInt64(x.left.location(), CheckValue(x.left));
  ExpressionInfo right = EnsureInt64(x.right.location(), CheckValue(x.right));
  return ExpressionInfo{
      .code = ir::Sequence({std::move(left.code), std::move(right.code)}),
      .value = TypedExpression(Category::kRvalue, ir::Scalar::kInt64,
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
  const ir::Label if_false = context_->Label(".Lternary_else");
  const ir::Label end = context_->Label(".Lternary_end");
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

ir::Code CheckBlock(ModuleContext& context, Environment& parent_environment,
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

ir::Code StatementChecker::operator()(const ast::Import& x) {
  throw Error(x.location, "import statements must appear at module scope");
}

ir::Code StatementChecker::operator()(const ast::Export& x) {
  throw Error(x.location, "export statements must appear at module scope");
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
  return ir::Sequence({std::move(left.code), std::move(right.code),
                       DoStore(x.location, std::move(left.value.value),
                               std::move(left.value.type), std::move(right))});
}

ir::Code StatementChecker::operator()(const ast::DeclareAndAssign& x) {
  ir::Type type;
  if (x.type) type = CheckType(*x.type);
  ExpressionInfo value = CheckValue(x.value);
  if (!x.type) type = value.value.type;
  const ir::Local::Offset offset = frame_->Allocate(type);
  environment_->Define(
      x.name, Environment::Definition{
                  .location = x.location,
                  .value = TypedExpression(Category::kLvalue, type,
                                           Representation::kAddress,
                                           ir::Local(offset))});
  return ir::Sequence({
      std::move(value.code),
      DoStore(x.location, ir::Local(offset), std::move(type),
              std::move(value))});
}

ir::Code StatementChecker::operator()(const ast::If& x) {
  ExpressionInfo condition =
      EnsureComparable(x.condition.location(), CheckValue(x.condition));
  ir::Code then_branch = CheckBlock(x.then_branch);
  ir::Code else_branch = CheckBlock(x.else_branch);
  const ir::Label if_false = context_->Label(".Lif_false");
  const ir::Label end = context_->Label(".Lif_end");
  return ir::Sequence(
      {std::move(condition.code),
       ir::JumpUnless(std::move(condition.value.value), if_false),
       std::move(then_branch), ir::Jump(end), if_false, std::move(else_branch),
       end});
}

ir::Code StatementChecker::operator()(const ast::While& x) {
  const ir::Label loop_start = context_->Label(".Lwhile_start");
  const ir::Label loop_condition = context_->Label(".Lwhile_condition");
  const ir::Label loop_end = context_->Label(".Lwhile_end");
  ExpressionInfo condition =
      EnsureInt64(x.condition.location(), CheckValue(x.condition));
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
  if (x.value) {
    ExpressionInfo value = CheckValue(*x.value);
    return ir::Sequence(
        {std::move(value.code),
         DoStore(x.location, ir::Load64(ir::Local(ir::Local::Offset{16})),
                 environment_->FunctionType()->return_type, std::move(value)),
         ir::Return()});
  } else {
    if (environment_->FunctionType()->return_type != ir::Void{}) {
      throw Error(x.location,
                  "cannot return void from a function with return type ",
                  environment_->FunctionType()->return_type);
    }
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
  auto label = environment_->Continue();
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

ir::Code StatementChecker::operator()(const ast::StructDefinition& x) {
  throw Error(x.location, "function-local struct definitions are forbidden");
}

ExpressionInfo StatementChecker::CheckValue(const ast::Expression& x) {
  return aoc2021::CheckValue(*context_, *environment_, *frame_, x);
}

ir::Type StatementChecker::CheckType(const ast::Expression& x) {
  return aoc2021::CheckType(*context_, *environment_, *frame_, x);
}

ir::Code StatementChecker::CheckBlock(Environment& parent_environment,
                                      std::span<const ast::Statement> block) {
  return aoc2021::CheckBlock(*context_, parent_environment, *frame_, block);
}

ir::Code StatementChecker::CheckBlock(std::span<const ast::Statement> block) {
  return CheckBlock(*environment_, block);
}

ir::Code ModuleStatementChecker::operator()(const ast::Import& x) {
  std::filesystem::path import_path = context_->Path().parent_path() / x.path;
  environment_->Define(
      x.alias.value,
      Environment::Definition{
          .location = x.location,
          .value = TypedExpression(
              Category::kRvalue, ir::Module(std::move(import_path)),
              Representation::kDirect, ir::IntegerLiteral(0))});
  return ir::Sequence();
}

ir::Code ModuleStatementChecker::operator()(const ast::Export& x) {
  if (environment_ == &context_->ExportedEnvironment()) {
    throw Error(x.location, "redundant export keyword");
  }
  ModuleStatementChecker checker(*context_, context_->ExportedEnvironment());
  return std::visit(checker, x.target->value);
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

ir::Code ModuleStatementChecker::operator()(const ast::DeclareAndAssign& x) {
  throw Error(x.location,
              "declarations with assignments are forbidden at module scope");
}

ir::Code ModuleStatementChecker::operator()(const ast::If& x) {
  throw Error(x.location, "if statements are forbidden at module scope");
}

ir::Code ModuleStatementChecker::operator()(const ast::While& x) {
  throw Error(x.location, "while statements are forbidden at module scope");
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

ir::Code ModuleStatementChecker::operator()(const ast::DiscardedExpression& x) {
  throw Error(x.location,
              "discarded expressions are forbidden at module scope");
}

ir::Code ModuleStatementChecker::operator()(const ast::FunctionDefinition& x) {
  const ir::Label function = context_->Label(x.name);
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
  function_environment.SetFunctionType(
      ir::FunctionPointer(return_type, parameters));
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
    function_environment.Define(
        parameter.name.value,
        Environment::Definition{
            .location = parameter.name.location,
            .value = AccessParameter(parameter.name.location, parameters[i],
                                     8 * (i + args_begin))});
  }

  FrameAllocator frame;
  ir::Code code = CheckBlock(*context_, function_environment, frame, x.body);
  return ir::Sequence({function, ir::BeginFrame(frame.max_size()),
                       std::move(code), ir::Return()});
}

ir::Code ModuleStatementChecker::operator()(const ast::StructDefinition& x) {
  // TODO: Merge this functionality with FrameAllocator.
  std::int64_t size = 0, alignment = 1;
  std::map<std::string, ir::Struct::Field> fields;
  for (const auto& field : x.fields) {
    ir::Type type = CheckType(field.type);
    alignment = std::max(alignment, ir::Alignment(type));
    // Pad to the alignment.
    size = (size + alignment - 1) / alignment * alignment;
    const std::int64_t offset = size;
    size += ir::Size(type);
    auto [i, is_new] = fields.emplace(
        field.name.value, ir::Struct::Field(std::move(type), offset));
    if (!is_new) throw Error(field.name.location, "duplicate field name");
  }
  // Pad the size of the struct to be a multiple of its alignment.
  size = (size + alignment - 1) / alignment * alignment;
  environment_->Define(
      x.name, Environment::Definition{
                  .location = x.location,
                  .value = ir::Struct(context_->Struct(), x.name,
                                      std::move(fields), size, alignment)});
  return ir::Sequence();
}

ir::Type ModuleStatementChecker::CheckType(const ast::Expression& x) {
  // All expressions at global scope must be constant expressions, so the frame
  // is not actually needed at runtime.
  FrameAllocator frame;
  return aoc2021::CheckType(*context_, *environment_, frame, x);
}

}  // namespace

void Environment::Define(std::string_view name, Definition definition) {
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

const ir::Label* Environment::Break() const noexcept {
  return break_ ? &*break_ : parent_ ? parent_->Break() : nullptr;
}

const ir::Label* Environment::Continue() const noexcept {
  return continue_ ? &*continue_ : parent_ ? parent_->Continue() : nullptr;
}

const Environment::Definition* Environment::Lookup(
    std::string_view name) const {
  auto i = names_.find(name);
  if (i != names_.end()) return &i->second;
  return parent_ ? parent_->Lookup(name) : nullptr;
}

// Like Lookup(), except that it only searches within the broadest lexical
// scope that forbids shadowing: variables outside this scope are ignored.
const Environment::Definition* Environment::LookupWithinShadowDomain(
    std::string_view name) const {
  auto i = names_.find(name);
  if (i != names_.end()) return &i->second;
  return shadow_mode_ == ShadowMode::kDeny && parent_
             ? parent_->LookupWithinShadowDomain(name)
             : nullptr;
}

const ir::FunctionPointer* Environment::FunctionType() const {
  return function_type_ ? &*function_type_
         : parent_      ? parent_->FunctionType()
                        : nullptr;
}

const Environment& Checker::EnvironmentFor(const std::filesystem::path& path) {
  return EntryFor(path).exports;
}

const ir::Unit& Checker::Check(const std::filesystem::path& path) {
  return *EntryFor(path).ir;
}

const Checker::Entry& Checker::EntryFor(const std::filesystem::path& path) {
  auto [i, is_new] = entries_.emplace(
      std::filesystem::absolute(path),
      Entry{.path = std::filesystem::absolute(path),
            .source = std::nullopt,
            .ast = std::nullopt,
            .ir = std::nullopt,
            .exports = Environment(BuiltinEnvironment(),
                                   Environment::ShadowMode::kDeny)});
  if (!is_new) {
    if (i->second.ir) return i->second;
    throw std::runtime_error("circular include dependency");
  }
  assert(is_new);
  Entry& entry = i->second;
  entry.source.emplace(loader_(path.native()));
  entry.ast.emplace(aoc2021::Parser(*entry.source).ParseProgram());
  ModuleContext context(*this, entry);
  std::vector<ir::Code> code;
  for (const auto& statement : *entry.ast) {
    ModuleStatementChecker checker(context, context.UnexportedEnvironment());
    code.push_back(std::visit(checker, statement->value));
  }
  entry.ir.emplace(
      ir::Unit{.main = context.Main(),
               .data = context.Globals(),
               .string_literals = context.StringLiterals(),
               .code = ir::Flatten(ir::Sequence(std::move(code)))});
  return entry;
}

ir::Unit Checker::Finish() {
  struct Main {
    std::filesystem::path path;
    ir::Label label;
  };
  std::vector<Main> mains;
  std::map<ir::Global, std::int64_t> data;
  std::map<ir::Global, std::string> string_literals;
  std::vector<ir::Code> code;

  for (auto& [path, entry] : entries_) {
    ir::Unit& unit = entry.ir.value();
    if (unit.main) mains.push_back(Main(path, *unit.main));
    data.merge(unit.data);
    string_literals.merge(unit.string_literals);
    code.push_back(std::move(unit.code));
  }
  entries_.clear();

  if (mains.empty()) throw std::runtime_error("no main function");

  if (mains.size() > 1) {
    std::ostringstream output;
    output << "error: multiple main functions:\n";
    for (const auto& main : mains) output << main.path << '\n';
    throw std::runtime_error(output.str());
  }

  return ir::Unit{.main = std::move(mains.front().label),
                  .data = std::move(data),
                  .string_literals = std::move(string_literals),
                  .code = ir::Sequence(std::move(code))};
}

}  // namespace aoc2021
