#ifndef IR_H_
#define IR_H_

#include <concepts>
#include <memory>

namespace aoc2021::ir {

template <typename T>
struct StepVisitor;

template <typename T>
concept Step = std::invocable<StepVisitor<void>&, const T&>;

class AnyStep {
 public:
  // Implicit conversion from any type of expression.
  template <Step T>
  AnyStep(T value) noexcept : value_(new Adaptor<T>(std::move(value))) {}

  void Visit(StepVisitor<void>& visitor) const {
    return value_->Visit(visitor);
  }

  template <typename T>
  T Visit(StepVisitor<T>& visitor) const;

  explicit operator bool() const noexcept { return value_ != nullptr; }

 private:
  struct Interface {
    virtual ~Interface() = default;
    virtual void Visit(StepVisitor<void>&) const = 0;
  };

  template <Step T>
  class Adaptor : public Interface {
   public:
    explicit Adaptor(T value) noexcept : value_(std::move(value)) {}

    void Visit(StepVisitor<void>& visitor) const override {
      visitor(value_);
    }

   private:
    T value_;
  };

  std::unique_ptr<Interface> value_;
};

std::ostream& operator<<(std::ostream&, const AnyStep&) noexcept;

// Represents the address of a global variable.
struct Global { std::string name; };

// Represents the address of a local variable as its offset from the frame
// pointer.
struct Local { std::int64_t offset; };

// Pops an address and loads a 64-bit value from it.
struct Load64 {};

// Pops an address, pops a 64-bit value, stores the value to the address.
struct Store64 {};

// Push a constant integer.
struct IntegerLiteral { std::int64_t value; };

// Pop a function pointer, call it with the given number of arguments.
struct Call { std::int64_t num_arguments; };

// Pure calculation. Unary expressions pop an argument and push a result. Binary
// expressions pop b, then a, then push f(a, b).
struct Negate {};
struct LogicalNot {};
struct BitwiseNot {};
struct Add {};
struct Subtract {};
struct Multiply {};
struct Divide {};
struct Modulo {};
struct LessThan {};
struct LessOrEqual {};
struct Equal {};
struct NotEqual {};
struct BitwiseAnd {};
struct BitwiseOr {};
struct BitwiseXor {};
struct ShiftLeft {};
struct ShiftRight {};

template <typename T>
struct StepVisitor {
  virtual ~StepVisitor() = default;
  virtual T operator()(const Global&) = 0;
  virtual T operator()(const Local&) = 0;
  virtual T operator()(const Load64&) = 0;
  virtual T operator()(const Store64&) = 0;
  virtual T operator()(const IntegerLiteral&) = 0;
  virtual T operator()(const Call&) = 0;
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

}  // namespace aoc2021::ir

#endif  // IR_H_
