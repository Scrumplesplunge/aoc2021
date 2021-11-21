#ifndef IR_H_
#define IR_H_

#include <concepts>
#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "variant_utils.h"

namespace aoc2021::ir {

struct ExpressionVariant;

class Expression {
 public:
  // Implicit conversion from any type of expression.
  template <HoldableBy<ExpressionVariant> T>
  Expression(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const ExpressionVariant& operator*() const noexcept;
  const ExpressionVariant* operator->() const noexcept { return &**this; }

  bool operator==(const Expression&) const;
  std::strong_ordering operator<=>(const Expression&) const;

 private:
  std::shared_ptr<const ExpressionVariant> value_;
};

// Represents an instruction address, such as a function address or jump target.
struct Label {
  bool operator==(const Label&) const = default;
  auto operator<=>(const Label&) const = default;

  explicit Label(std::string_view name) noexcept;
  Label(std::string_view prefix, std::int64_t suffix) noexcept;
  std::string value;
};

// Represents the address of a global variable with the given ID.
struct Global {
  bool operator==(const Global&) const = default;
  auto operator<=>(const Global&) const = default;

  Global(std::string_view prefix, std::int64_t suffix);
  std::string value;
};

// Represents the address of a local variable, identified by its offset from the
// frame pointer.
struct Local {
  bool operator==(const Local&) const = default;
  auto operator<=>(const Local&) const = default;

  enum class Offset : std::int64_t {};
  Offset offset;
};

// Loads an 8-bit value from the given address.
struct Load8 {
  bool operator==(const Load8&) const = default;
  auto operator<=>(const Load8&) const = default;

  Expression address;
};

// Loads a 64-bit value from the given address.
struct Load64 {
  bool operator==(const Load64&) const = default;
  auto operator<=>(const Load64&) const = default;

  Expression address;
};

// Represents a literal value.
struct IntegerLiteral {
  bool operator==(const IntegerLiteral&) const = default;
  auto operator<=>(const IntegerLiteral&) const = default;

  std::int64_t value;
};

// Pure calculation.
struct Negate {
  bool operator==(const Negate&) const = default;
  auto operator<=>(const Negate&) const = default;

  Expression inner;
};

struct LogicalNot {
  bool operator==(const LogicalNot&) const = default;
  auto operator<=>(const LogicalNot&) const = default;

  Expression inner;
};

struct BitwiseNot {
  bool operator==(const BitwiseNot&) const = default;
  auto operator<=>(const BitwiseNot&) const = default;

  Expression inner;
};

struct Add {
  bool operator==(const Add&) const = default;
  auto operator<=>(const Add&) const = default;

  Expression left, right;
};

struct Subtract {
  bool operator==(const Subtract&) const = default;
  auto operator<=>(const Subtract&) const = default;

  Expression left, right;
};

struct Multiply {
  bool operator==(const Multiply&) const = default;
  auto operator<=>(const Multiply&) const = default;

  Expression left, right;
};

struct Divide {
  bool operator==(const Divide&) const = default;
  auto operator<=>(const Divide&) const = default;

  Expression left, right;
};

struct Modulo {
  bool operator==(const Modulo&) const = default;
  auto operator<=>(const Modulo&) const = default;

  Expression left, right;
};

struct LessThan {
  bool operator==(const LessThan&) const = default;
  auto operator<=>(const LessThan&) const = default;

  Expression left, right;
};

struct LessOrEqual {
  bool operator==(const LessOrEqual&) const = default;
  auto operator<=>(const LessOrEqual&) const = default;

  Expression left, right;
};

struct Equal {
  bool operator==(const Equal&) const = default;
  auto operator<=>(const Equal&) const = default;

  Expression left, right;
};

struct NotEqual {
  bool operator==(const NotEqual&) const = default;
  auto operator<=>(const NotEqual&) const = default;

  Expression left, right;
};

struct BitwiseAnd {
  bool operator==(const BitwiseAnd&) const = default;
  auto operator<=>(const BitwiseAnd&) const = default;

  Expression left, right;
};

struct BitwiseOr {
  bool operator==(const BitwiseOr&) const = default;
  auto operator<=>(const BitwiseOr&) const = default;

  Expression left, right;
};

struct BitwiseXor {
  bool operator==(const BitwiseXor&) const = default;
  auto operator<=>(const BitwiseXor&) const = default;

  Expression left, right;
};

struct ShiftLeft {
  bool operator==(const ShiftLeft&) const = default;
  auto operator<=>(const ShiftLeft&) const = default;

  Expression left, right;
};

struct ShiftRight {
  bool operator==(const ShiftRight&) const = default;
  auto operator<=>(const ShiftRight&) const = default;

  Expression left, right;
};

struct ExpressionVariant {
  bool operator==(const ExpressionVariant&) const = default;
  auto operator<=>(const ExpressionVariant&) const = default;

  std::variant<Label, Global, Local, Load8, Load64, IntegerLiteral, Negate,
               LogicalNot, BitwiseNot, Add, Subtract, Multiply, Divide, Modulo,
               LessThan, LessOrEqual, Equal, NotEqual, BitwiseAnd, BitwiseOr,
               BitwiseXor, ShiftLeft, ShiftRight>
      value;
};

template <HoldableBy<ExpressionVariant> T>
Expression::Expression(T value) noexcept
    : value_(std::make_shared<ExpressionVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const Label&) noexcept;
std::ostream& operator<<(std::ostream&, const Global&) noexcept;
std::ostream& operator<<(std::ostream&, const Local&) noexcept;
std::ostream& operator<<(std::ostream&, const Load8&) noexcept;
std::ostream& operator<<(std::ostream&, const Load64&) noexcept;
std::ostream& operator<<(std::ostream&, const IntegerLiteral&) noexcept;
std::ostream& operator<<(std::ostream&, const Negate&) noexcept;
std::ostream& operator<<(std::ostream&, const LogicalNot&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseNot&) noexcept;
std::ostream& operator<<(std::ostream&, const Add&) noexcept;
std::ostream& operator<<(std::ostream&, const Subtract&) noexcept;
std::ostream& operator<<(std::ostream&, const Multiply&) noexcept;
std::ostream& operator<<(std::ostream&, const Divide&) noexcept;
std::ostream& operator<<(std::ostream&, const Modulo&) noexcept;
std::ostream& operator<<(std::ostream&, const LessThan&) noexcept;
std::ostream& operator<<(std::ostream&, const LessOrEqual&) noexcept;
std::ostream& operator<<(std::ostream&, const Equal&) noexcept;
std::ostream& operator<<(std::ostream&, const NotEqual&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseAnd&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseOr&) noexcept;
std::ostream& operator<<(std::ostream&, const BitwiseXor&) noexcept;
std::ostream& operator<<(std::ostream&, const ShiftLeft&) noexcept;
std::ostream& operator<<(std::ostream&, const ShiftRight&) noexcept;
std::ostream& operator<<(std::ostream&, const Expression&) noexcept;

struct TypeVariant;

class Type {
 public:
  Type() noexcept = default;

  // Implicit conversion from any type of expression.
  template <HoldableBy<TypeVariant> T>
  Type(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const TypeVariant& operator*() const noexcept;
  const TypeVariant* operator->() const noexcept { return &**this; }

  bool operator==(const Type&) const;
  std::strong_ordering operator<=>(const Type&) const;

 private:
  std::shared_ptr<const TypeVariant> value_;
};

enum class Void { kVoid };
enum class Scalar { kByte, kInt64 };

struct Pointer {
  bool operator==(const Pointer&) const = default;
  auto operator<=>(const Pointer&) const = default;

  Type pointee;
};

struct FunctionPointer {
  bool operator==(const FunctionPointer&) const = default;
  auto operator<=>(const FunctionPointer&) const = default;

  Type return_type;
  std::vector<Type> parameters;
};

struct Array {
  bool operator==(const Array&) const = default;
  auto operator<=>(const Array&) const = default;

  std::int64_t size;
  Type element;
};

struct Span {
  bool operator==(const Span&) const = default;
  auto operator<=>(const Span&) const = default;

  Type element;
};

struct Module {
  bool operator==(const Module&) const = default;
  auto operator<=>(const Module&) const = default;

  std::filesystem::path path;
};

struct TypeVariant {
  bool operator==(const TypeVariant&) const = default;
  auto operator<=>(const TypeVariant&) const = default;

  std::variant<Void, Scalar, Pointer, FunctionPointer, Array, Span, Module>
      value;
};

template <HoldableBy<TypeVariant> T>
Type::Type(T value) noexcept
    : value_(std::make_shared<TypeVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, Void) noexcept;
std::ostream& operator<<(std::ostream&, Scalar) noexcept;
std::ostream& operator<<(std::ostream&, const Pointer&) noexcept;
std::ostream& operator<<(std::ostream&, const FunctionPointer&) noexcept;
std::ostream& operator<<(std::ostream&, const Array&) noexcept;
std::ostream& operator<<(std::ostream&, const Span&) noexcept;
std::ostream& operator<<(std::ostream&, const Module&) noexcept;
std::ostream& operator<<(std::ostream&, const Type&) noexcept;

std::int64_t Size(Void) noexcept;
std::int64_t Size(Scalar) noexcept;
std::int64_t Size(const Pointer&) noexcept;
std::int64_t Size(const Array&) noexcept;
std::int64_t Size(const Span&) noexcept;
std::int64_t Size(const Module&) noexcept;
std::int64_t Size(const Type&) noexcept;

std::int64_t Alignment(Void) noexcept;
std::int64_t Alignment(Scalar) noexcept;
std::int64_t Alignment(const Pointer&) noexcept;
std::int64_t Alignment(const Array&) noexcept;
std::int64_t Alignment(const Span&) noexcept;
std::int64_t Alignment(const Module&) noexcept;
std::int64_t Alignment(const Type&) noexcept;

struct CodeVariant;

class Code {
 public:
  // Implicit conversion from any type of expression.
  template <HoldableBy<CodeVariant> T>
  Code(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const CodeVariant& operator*() const noexcept;
  const CodeVariant* operator->() const noexcept { return &**this; }

  bool operator==(const Code&) const;
  std::strong_ordering operator<=>(const Code&) const;

 private:
  std::shared_ptr<const CodeVariant> value_;
};

// Stores an 8-bit value to the given address.
struct Store8 {
  bool operator==(const Store8&) const = default;
  auto operator<=>(const Store8&) const = default;

  Expression address, value;
};

// Stores a 64-bit value to the given address.
struct Store64 {
  bool operator==(const Store64&) const = default;
  auto operator<=>(const Store64&) const = default;

  Expression address, value;
};

// Call a function with the given arguments.
struct Call {
  bool operator==(const Call&) const = default;
  auto operator<=>(const Call&) const = default;

  Expression function_address;
  std::vector<Expression> arguments;
};

// Start a function stack frame: set up the frame pointer and adjust the stack.
struct BeginFrame {
  bool operator==(const BeginFrame&) const = default;
  auto operator<=>(const BeginFrame&) const = default;

  std::int64_t size;
};

struct Return {
  bool operator==(const Return&) const = default;
  auto operator<=>(const Return&) const = default;
};

struct Jump {
  bool operator==(const Jump&) const = default;
  auto operator<=>(const Jump&) const = default;

  Label target;
};

struct JumpIf {
  bool operator==(const JumpIf&) const = default;
  auto operator<=>(const JumpIf&) const = default;

  Expression condition;
  Label target;
};

struct JumpUnless {
  bool operator==(const JumpUnless&) const = default;
  auto operator<=>(const JumpUnless&) const = default;

  Expression condition;
  Label target;
};

struct Sequence {
  bool operator==(const Sequence&) const = default;
  auto operator<=>(const Sequence&) const = default;

  std::vector<Code> value;
};

struct CodeVariant {
  bool operator==(const CodeVariant&) const = default;
  auto operator<=>(const CodeVariant&) const = default;

  std::variant<Label, Store8, Store64, Call, BeginFrame, Return, Jump, JumpIf,
               JumpUnless, Sequence>
      value;
};

template <HoldableBy<CodeVariant> T>
Code::Code(T value) noexcept
    : value_(std::make_shared<CodeVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const Store8&) noexcept;
std::ostream& operator<<(std::ostream&, const Store64&) noexcept;
std::ostream& operator<<(std::ostream&, const Call&) noexcept;
std::ostream& operator<<(std::ostream&, const BeginFrame&) noexcept;
std::ostream& operator<<(std::ostream&, const Return&) noexcept;
std::ostream& operator<<(std::ostream&, const Jump&) noexcept;
std::ostream& operator<<(std::ostream&, const JumpIf&) noexcept;
std::ostream& operator<<(std::ostream&, const JumpUnless&) noexcept;
std::ostream& operator<<(std::ostream&, const Sequence&) noexcept;
std::ostream& operator<<(std::ostream&, const Code&) noexcept;

Sequence Flatten(const Code& code);

struct Unit {
  std::optional<ir::Label> main;
  std::map<ir::Global, std::int64_t> data;
  std::map<ir::Global, std::string> string_literals;
  ir::Code code;
};

}  // namespace aoc2021::ir

#endif  // IR_H_
