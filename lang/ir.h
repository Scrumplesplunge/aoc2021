#ifndef IR_H_
#define IR_H_

#include <concepts>
#include <memory>
#include <optional>
#include <vector>

namespace aoc2021::ir {

template <typename T>
struct ExpressionVisitor;

template <typename T>
concept Expression =
    std::copy_constructible<T> &&
    std::invocable<ExpressionVisitor<void>&, const T&>;

class AnyExpression {
 public:
  // Implicit conversion from any type of expression.
  template <Expression T>
  AnyExpression(T value) noexcept : value_(new Adaptor<T>(std::move(value))) {}

  AnyExpression(AnyExpression&&) noexcept = default;
  AnyExpression& operator=(AnyExpression&&) noexcept = default;

  AnyExpression(const AnyExpression& other) : value_(other.value_->Copy()) {}
  AnyExpression& operator=(const AnyExpression& other) {
    if (this != &other) value_.reset(other.value_->Copy());
    return *this;
  }

  void Visit(ExpressionVisitor<void>& visitor) const {
    return value_->Visit(visitor);
  }

  template <typename T>
  T Visit(ExpressionVisitor<T>& visitor) const;

  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  struct Interface {
    virtual ~Interface() = default;
    virtual void Visit(ExpressionVisitor<void>&) const = 0;
    virtual Interface* Copy() const = 0;
  };

  template <Expression T>
  class Adaptor : public Interface {
   public:
    explicit Adaptor(T value) noexcept : value_(std::move(value)) {}

    void Visit(ExpressionVisitor<void>& visitor) const override {
      visitor(value_);
    }

    Adaptor* Copy() const override { return new Adaptor(value_); }

   private:
    T value_;
  };

  std::unique_ptr<Interface> value_;
};

std::ostream& operator<<(std::ostream&, const AnyExpression&) noexcept;

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

template <typename T>
struct ExpressionVisitor {
  virtual ~ExpressionVisitor() = default;
  virtual T operator()(const Label&) = 0;
  virtual T operator()(const Global&) = 0;
  virtual T operator()(const Local&) = 0;
  virtual T operator()(const Load64&) = 0;
  virtual T operator()(const IntegerLiteral&) = 0;
  virtual T operator()(const Negate&) = 0;
  virtual T operator()(const LogicalNot&) = 0;
  virtual T operator()(const BitwiseNot&) = 0;
  virtual T operator()(const Add&) = 0;
  virtual T operator()(const Subtract&) = 0;
  virtual T operator()(const Multiply&) = 0;
  virtual T operator()(const Divide&) = 0;
  virtual T operator()(const Modulo&) = 0;
  virtual T operator()(const LessThan&) = 0;
  virtual T operator()(const LessOrEqual&) = 0;
  virtual T operator()(const Equal&) = 0;
  virtual T operator()(const NotEqual&) = 0;
  virtual T operator()(const BitwiseAnd&) = 0;
  virtual T operator()(const BitwiseOr&) = 0;
  virtual T operator()(const BitwiseXor&) = 0;
  virtual T operator()(const ShiftLeft&) = 0;
  virtual T operator()(const ShiftRight&) = 0;
};

template <typename T>
T AnyExpression::Visit(ExpressionVisitor<T>& visitor) const {
  struct ProxyVisitor : ExpressionVisitor<void> {
    ProxyVisitor(ExpressionVisitor<T>& f) noexcept : f(f) {}
    void operator()(const Label& x) override { new (result) T(f(x)); }
    void operator()(const Global& x) override { new (result) T(f(x)); }
    void operator()(const Local& x) override { new (result) T(f(x)); }
    void operator()(const Load64& x) override { new (result) T(f(x)); }
    void operator()(const IntegerLiteral& x) override { new (result) T(f(x)); }
    void operator()(const Negate& x) override { new (result) T(f(x)); }
    void operator()(const LogicalNot& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseNot& x) override { new (result) T(f(x)); }
    void operator()(const Add& x) override { new (result) T(f(x)); }
    void operator()(const Subtract& x) override { new (result) T(f(x)); }
    void operator()(const Multiply& x) override { new (result) T(f(x)); }
    void operator()(const Divide& x) override { new (result) T(f(x)); }
    void operator()(const Modulo& x) override { new (result) T(f(x)); }
    void operator()(const LessThan& x) override { new (result) T(f(x)); }
    void operator()(const LessOrEqual& x) override { new (result) T(f(x)); }
    void operator()(const Equal& x) override { new (result) T(f(x)); }
    void operator()(const NotEqual& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseAnd& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseOr& x) override { new (result) T(f(x)); }
    void operator()(const BitwiseXor& x) override { new (result) T(f(x)); }
    void operator()(const ShiftLeft& x) override { new (result) T(f(x)); }
    void operator()(const ShiftRight& x) override { new (result) T(f(x)); }

    T Consume() && { return std::move(*(T*)result); }

    ExpressionVisitor<T>& f;
    alignas(T) char result[sizeof(T)];
  };
  ProxyVisitor v{visitor};
  Visit(v);
  return std::move(v).Consume();
}

template <typename T>
struct CodeVisitor;

template <typename T>
concept Code = std::invocable<CodeVisitor<void>&, const T&>;

class AnyCode {
 public:
  // Implicit conversion from any type of expression.
  template <Code T>
  AnyCode(T value) noexcept : value_(new Adaptor<T>(std::move(value))) {}

  AnyCode(AnyCode&&) noexcept = default;
  AnyCode& operator=(AnyCode&&) noexcept = default;

  AnyCode(const AnyCode& other) : value_(other.value_->Copy()) {}
  AnyCode& operator=(const AnyCode& other) {
    if (this != &other) value_.reset(other.value_->Copy());
    return *this;
  }

  void Visit(CodeVisitor<void>& visitor) const {
    return value_->Visit(visitor);
  }

  template <typename T>
  T Visit(CodeVisitor<T>& visitor) const;

  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  struct Interface {
    virtual ~Interface() = default;
    virtual void Visit(CodeVisitor<void>&) const = 0;
    virtual Interface* Copy() const = 0;
  };

  template <Code T>
  class Adaptor : public Interface {
   public:
    explicit Adaptor(T value) noexcept : value_(std::move(value)) {}

    void Visit(CodeVisitor<void>& visitor) const override {
      visitor(value_);
    }

    Adaptor* Copy() const override { return new Adaptor(value_); }

   private:
    T value_;
  };

  std::unique_ptr<Interface> value_;
};

std::ostream& operator<<(std::ostream&, const AnyCode&) noexcept;

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

template <typename T>
struct CodeVisitor {
  virtual ~CodeVisitor() = default;
  virtual T operator()(const Label&) = 0;
  virtual T operator()(const Store64&) = 0;
  virtual T operator()(const StoreCall64&) = 0;
  virtual T operator()(const BeginFrame&) = 0;
  virtual T operator()(const Return&) = 0;
  virtual T operator()(const Jump&) = 0;
  virtual T operator()(const JumpIf&) = 0;
  virtual T operator()(const JumpUnless&) = 0;
  virtual T operator()(const Sequence&) = 0;
};

Sequence Flatten(const AnyCode& code);

struct Unit {
  std::optional<ir::Label> main;
  ir::AnyCode code;
};

}  // namespace aoc2021::ir

#endif  // IR_H_
