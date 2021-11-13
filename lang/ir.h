#ifndef IR_H_
#define IR_H_

#include <concepts>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "variant_utils.h"

namespace aoc2021::ir {

struct ExpressionVariant;

class AnyExpression {
 public:
  // Implicit conversion from any type of expression.
  template <HoldableBy<ExpressionVariant> T>
  AnyExpression(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const ExpressionVariant& operator*() const noexcept;
  const ExpressionVariant* operator->() const noexcept { return &**this; }

 private:
  std::shared_ptr<const ExpressionVariant> value_;
};

// Represents an instruction address, such as a function address or jump target.
struct Label {
  Label(std::string_view prefix, std::int64_t suffix);
  std::string value;
};

// Represents the address of a global variable with the given ID.
struct Global {
  Global(std::string_view prefix, std::int64_t suffix);
  std::string value;
};

// Represents the address of a local variable, identified by its offset from the
// frame pointer.
struct Local {
  enum class Offset : std::int64_t {};
  Offset offset;
};

// Loads a 64-bit value from the given address.
struct Load64 { AnyExpression address; };

// Represents a literal value.
struct IntegerLiteral { std::int64_t value; };

// Pure calculation.
struct Negate { AnyExpression inner; };
struct LogicalNot { AnyExpression inner; };
struct BitwiseNot { AnyExpression inner; };
struct Add { AnyExpression left, right; };
struct Subtract { AnyExpression left, right; };
struct Multiply { AnyExpression left, right; };
struct Divide { AnyExpression left, right; };
struct Modulo { AnyExpression left, right; };
struct LessThan { AnyExpression left, right; };
struct LessOrEqual { AnyExpression left, right; };
struct Equal { AnyExpression left, right; };
struct NotEqual { AnyExpression left, right; };
struct BitwiseAnd { AnyExpression left, right; };
struct BitwiseOr { AnyExpression left, right; };
struct BitwiseXor { AnyExpression left, right; };
struct ShiftLeft { AnyExpression left, right; };
struct ShiftRight { AnyExpression left, right; };

struct ExpressionVariant {
  auto operator<=>(const ExpressionVariant&) const = default;

  std::variant<Label, Global, Local, Load64, IntegerLiteral, Negate, LogicalNot,
               BitwiseNot, Add, Subtract, Multiply, Divide, Modulo, LessThan,
               LessOrEqual, Equal, NotEqual, BitwiseAnd, BitwiseOr, BitwiseXor,
               ShiftLeft, ShiftRight>
      value;
};

template <HoldableBy<ExpressionVariant> T>
AnyExpression::AnyExpression(T value) noexcept
    : value_(std::make_shared<ExpressionVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const Label&) noexcept;
std::ostream& operator<<(std::ostream&, const Global&) noexcept;
std::ostream& operator<<(std::ostream&, const Local&) noexcept;
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
std::ostream& operator<<(std::ostream&, const AnyExpression&) noexcept;

struct CodeVariant;

class AnyCode {
 public:
  // Implicit conversion from any type of expression.
  template <HoldableBy<CodeVariant> T>
  AnyCode(T value) noexcept;

  explicit operator bool() const noexcept { return value_ != nullptr; }
  const CodeVariant& operator*() const noexcept;
  const CodeVariant* operator->() const noexcept { return &**this; }

 private:
  std::shared_ptr<const CodeVariant> value_;
};

// Pops an address, pops a 64-bit value, stores the value to the address.
struct Store64 { AnyExpression address, value; };

// Call a function with the given arguments, store the 64-bit result to the
// given address.
struct StoreCall64 {
  AnyExpression result_address;
  AnyExpression function_address;
  std::vector<AnyExpression> arguments;
};

// Start a function stack frame: set up the frame pointer and adjust the stack.
struct BeginFrame { std::int64_t size; };

struct Return { AnyExpression value; };

struct Jump { Label target; };

struct JumpIf {
  AnyExpression condition;
  Label target;
};

struct JumpUnless {
  AnyExpression condition;
  Label target;
};

struct Sequence {
  std::vector<AnyCode> value;
};

struct CodeVariant {
  auto operator<=>(const CodeVariant&) const = default;

  std::variant<Label, Store64, StoreCall64, BeginFrame, Return, Jump, JumpIf,
               JumpUnless, Sequence>
      value;
};

template <HoldableBy<CodeVariant> T>
AnyCode::AnyCode(T value) noexcept
    : value_(std::make_shared<CodeVariant>(std::move(value))) {}

std::ostream& operator<<(std::ostream&, const Store64&) noexcept;
std::ostream& operator<<(std::ostream&, const StoreCall64&) noexcept;
std::ostream& operator<<(std::ostream&, const BeginFrame&) noexcept;
std::ostream& operator<<(std::ostream&, const Return&) noexcept;
std::ostream& operator<<(std::ostream&, const Jump&) noexcept;
std::ostream& operator<<(std::ostream&, const JumpIf&) noexcept;
std::ostream& operator<<(std::ostream&, const JumpUnless&) noexcept;
std::ostream& operator<<(std::ostream&, const Sequence&) noexcept;
std::ostream& operator<<(std::ostream&, const AnyCode&) noexcept;

Sequence Flatten(const AnyCode& code);

struct Unit {
  std::optional<ir::Label> main;
  ir::AnyCode code;
};

}  // namespace aoc2021::ir

#endif  // IR_H_
