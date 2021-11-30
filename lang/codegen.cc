#include "codegen.h"

#include <bit>
#include <cassert>
#include <functional>
#include <iomanip>
#include <sstream>
#include <variant>

#include "string_utils.h"

namespace aoc2021 {
namespace {

enum class Register {
  kRax,
  kRbx,
  kRcx,
  kRdx,
  kRbp,
  kRsp,
  kRsi,
  kRdi,
  kR8,
  kR9,
  kR10,
  kR11,
  kR12,
  kR13,
  kR14,
  kR15,
};

const char* FullName(Register r) {
  switch (r) {
    case Register::kRax:
      return "%rax";
    case Register::kRbx:
      return "%rbx";
    case Register::kRcx:
      return "%rcx";
    case Register::kRdx:
      return "%rdx";
    case Register::kRbp:
      return "%rbp";
    case Register::kRsp:
      return "%rsp";
    case Register::kRsi:
      return "%rsi";
    case Register::kRdi:
      return "%rdi";
    case Register::kR8:
      return "%r8";
    case Register::kR9:
      return "%r9";
    case Register::kR10:
      return "%r10";
    case Register::kR11:
      return "%r11";
    case Register::kR12:
      return "%r12";
    case Register::kR13:
      return "%r13";
    case Register::kR14:
      return "%r14";
    case Register::kR15:
      return "%r15";
  }
  std::abort();
}

const char* ByteName(Register r) {
  switch (r) {
    case Register::kRax:
      return "%al";
    case Register::kRbx:
      return "%bl";
    case Register::kRcx:
      return "%cl";
    case Register::kRdx:
      return "%dl";
    case Register::kRbp:
      return "%bpl";
    case Register::kRsp:
      return "%spl";
    case Register::kRsi:
      return "%sil";
    case Register::kRdi:
      return "%dil";
    case Register::kR8:
      return "%r8b";
    case Register::kR9:
      return "%r9b";
    case Register::kR10:
      return "%r10b";
    case Register::kR11:
      return "%r11b";
    case Register::kR12:
      return "%r12b";
    case Register::kR13:
      return "%r13b";
    case Register::kR14:
      return "%r14b";
    case Register::kR15:
      return "%r15b";
  }
  std::abort();
}

std::ostream& operator<<(std::ostream& output, Register r) {
  return output << FullName(r);
}

constexpr Register kRegisterOrder[] = {
  // kRax is required by div
  Register::kRbx,
  // kRcx is required by asl/asr
  // kRdx is required by div
  // kRbp is required for stack frames
  // kRsp is required for the stack pointer
  Register::kRsi,
  Register::kRdi,
  Register::kR8,
  // kR9..kR15
};
constexpr int kNumWorkingRegisters = std::size(kRegisterOrder);

struct ProductionResult {
  Register result() const { return kRegisterOrder[registers_used - 1]; }

  // TODO: Add a cost field which can be used to decide between multiple
  // possible encodings.
  std::string code;
  int registers_used;
};

using Production =
    std::function<std::optional<ProductionResult>(const ir::Expression&)>;

std::optional<std::int64_t> GetConstant(const ir::Expression& x) {
  std::optional<std::int64_t> value;
  if (auto* i = std::get_if<ir::IntegerLiteral>(&x->value)) value = i->value;
  return value;
}

class ExpressionGenerator {
 public:
  const ProductionResult& Get(const ir::Expression& e) {
    if (auto i = cache_.find(&e); i != cache_.end()) return i->second;
    auto [i, is_new] = cache_.emplace(
        &e, std::visit([&](const auto& x) { return Produce(x); }, e->value));
    assert(is_new);
    return i->second;
  }

 private:
  void EnsureEnoughRegisters(int registers_used) {
    if (registers_used > kNumWorkingRegisters) {
      throw std::runtime_error(
          "error: code generator ran out of registers and register spilling "
          "has not been implemented yet.");
    }
  }
  ProductionResult Produce(const auto& x) { std::abort(); }

  ProductionResult Produce(const ir::Label& x) {
    return ProductionResult{
        .code = StrCat("  mov $", x.value, ", ", kRegisterOrder[0], "\n"),
        .registers_used = 1};
  }

  ProductionResult Produce(const ir::Global& x) {
    return ProductionResult{
        .code = StrCat("  mov $", x.value, ", ", kRegisterOrder[0], "\n"),
        .registers_used = 1};
  }

  ProductionResult Produce(const ir::Local& x) {
    return ProductionResult{.code = StrCat("  lea ", (std::int64_t)x.offset,
                                           "(%rbp), ", kRegisterOrder[0], "\n"),
                            .registers_used = 1};
  }

  ProductionResult Produce(const ir::Load8& x) {
    if (auto* local = std::get_if<ir::Local>(&x.address->value)) {
      // Directly load a byte from a local.
      return ProductionResult{
          .code = StrCat("  movzxb ", (std::int64_t)local->offset, "(%rbp), ",
                         kRegisterOrder[0], "\n"),
          .registers_used = 1};
    }
    if (auto* global = std::get_if<ir::Global>(&x.address->value)) {
      // Directly load a byte from a global.
      return ProductionResult{.code = StrCat("  movzxb ", global->value, ", ",
                                             kRegisterOrder[0], "\n"),
                              .registers_used = 1};
    }
    // Compute an address first and then load a byte from that address.
    const ProductionResult& address = Get(x.address);
    return ProductionResult{
        .code = StrCat(address.code, "  movzxb (", address.result(), "), ",
                       address.result(), "\n"),
        .registers_used = address.registers_used};
  }

  ProductionResult Produce(const ir::Load64& x) {
    if (auto* local = std::get_if<ir::Local>(&x.address->value)) {
      // Directly load a value from a local.
      return ProductionResult{
          .code = StrCat("  mov ", (std::int64_t)local->offset, "(%rbp), ",
                         kRegisterOrder[0], "\n"),
          .registers_used = 1};
    }
    if (auto* global = std::get_if<ir::Global>(&x.address->value)) {
      // Directly load a value from a global.
      return ProductionResult{.code = StrCat("  mov ", global->value, ", ",
                                             kRegisterOrder[0], "\n"),
                              .registers_used = 1};
    }
    // Compute an address first and then load a value from that address.
    const ProductionResult& address = Get(x.address);
    return ProductionResult{
        .code = StrCat(address.code, "  mov (", address.result(), "), ",
                       address.result(), "\n"),
        .registers_used = address.registers_used};
  }

  ProductionResult Produce(const ir::IntegerLiteral& x) {
    return ProductionResult{
        .code = StrCat("  mov $", x.value, ", ", kRegisterOrder[0], "\n"),
        .registers_used = 1};
  }

  ProductionResult Produce(const ir::Negate& x) {
    const ProductionResult& inner = Get(x.inner);
    return ProductionResult{
        .code = StrCat(inner.code, "  neg ", inner.result(), "\n"),
        .registers_used = inner.registers_used};
  }

  ProductionResult Produce(const ir::LogicalNot& x) {
    const ProductionResult& inner = Get(x.inner);
    // TODO: Find a way to avoid always using an extra register here.
    EnsureEnoughRegisters(inner.registers_used + 1);
    const Register result = kRegisterOrder[inner.registers_used];
    return ProductionResult{
        .code = StrCat(inner.code,
                       "  xor ", result, ", ", result, "\n",
                       "  test ", inner.result(), ", ", inner.result(), "\n",
                       "  sete ", ByteName(result), "\n"),
        .registers_used = inner.registers_used + 1};
  }

  ProductionResult Produce(const ir::BitwiseNot& x) {
    const ProductionResult& inner = Get(x.inner);
    return ProductionResult{
        .code = StrCat(inner.code, "  not ", inner.result(), "\n"),
        .registers_used = inner.registers_used};
  }

  ProductionResult Produce(const ir::Add& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    std::optional<std::int64_t> left_value = GetConstant(x.left);
    std::optional<std::int64_t> right_value = GetConstant(x.right);
    if (left_value && *left_value == 0) {
      // No-op addition.
      return right;
    } else if (right_value && *right_value == 0) {
      // No-op addition.
      return left;
    } else if (right_value) {
      // Add an immediate value.
      if (*right_value == 0) return left;
      return ProductionResult{.code = StrCat(left.code, "  add $", *right_value,
                                             ", ", left.result(), "\n"),
                              .registers_used = left.registers_used};
    } else if (left_value) {
      // Add an immediate value.
      if (*left_value == 0) return right;
      return ProductionResult{.code = StrCat(right.code, "  add $", *left_value,
                                             ", ", right.result(), "\n"),
                              .registers_used = right.registers_used};
    } else if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  add ", right.result(), ", ",
                         left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code = StrCat(right.code, left.code, "  add ", left.result(), ", ",
                         right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  add ", right.result(), ", ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  ProductionResult Produce(const ir::Subtract& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (auto right_value = GetConstant(x.right)) {
      // Subtract an immediate value.
      if (*right_value == 0) return left;
      return ProductionResult{.code = StrCat(left.code, "  sub $", *right_value,
                                             ", ", left.result(), "\n"),
                              .registers_used = left.registers_used};
    } else if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  sub ", right.result(), ", ",
                         left.result(), "\n"),
          .registers_used = left.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      const int registers_used =
          std::max(left.registers_used, right.registers_used) + 1;
      EnsureEnoughRegisters(registers_used);
      const Register result = kRegisterOrder[registers_used - 1];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  sub ", right.result(), ", ", result, "\n"),
          .registers_used = registers_used};
    }
  }

  ProductionResult Produce(const ir::Multiply& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    std::optional<std::int64_t> left_value = GetConstant(x.left);
    std::optional<std::int64_t> right_value = GetConstant(x.right);
    if (left_value && *left_value == 1) {
      // No-op multiplication.
      return right;
    } else if (right_value && *right_value == 1) {
      // No-op multiplication.
      return left;
    } else if (right_value) {
      // Multiply by an immediate value.
      return ProductionResult{
          .code = StrCat(left.code, "  imul $", *right_value, ", ",
                         left.result(), ", ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (left_value) {
      // Multiply by an immediate value.
      return ProductionResult{
          .code = StrCat(right.code, "  imul $", *left_value, ", ",
                         right.result(), ", ", right.result(), "\n"),
          .registers_used = right.registers_used};
    } else if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  imul ", right.result(), ", ",
                         left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code = StrCat(right.code, left.code, "  imul ", left.result(), ", ",
                         right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  imul ", right.result(), ", ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  ProductionResult Produce(const ir::Divide& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (auto right_value = GetConstant(x.right);
        *right_value > 0 &&
        std::has_single_bit(static_cast<std::uint64_t>(*right_value))) {
      const int shift =
          std::countr_zero(static_cast<std::uint64_t>(*right_value));
      // The divisor is a positive constant power of two, so we can use an
      // arithmetic shift instead of a division.
      return ProductionResult{.code = StrCat(left.code, "  asr $", shift, ", ",
                                             left.result(), "\n"),
                              .registers_used = left.registers_used};
    } else if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code =
              StrCat(left.code, right.code, "  mov ", left.result(), ", %rax\n",
                     "  xor %rdx, %rdx\n", "  idiv ", right.result(), "\n",
                     "  mov %rax, ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code =
              StrCat(right.code, left.code, "  mov ", left.result(), ", %rax\n",
                     "  xor %rdx, %rdx\n", "  idiv ", right.result(), "\n",
                     "  mov %rax, ", right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  mov ", result, ", %rax\n",
                     "  xor %rdx, %rdx\n", "  idiv ", right.result(), "\n",
                     "  mov %rax, ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
    }

  ProductionResult Produce(const ir::Modulo& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (auto right_value = GetConstant(x.right);
        *right_value > 0 &&
        std::has_single_bit(static_cast<std::uint64_t>(*right_value))) {
      // The divisor is a positive constant power of two, so we can use
      // a logical and instead of a division.
      return ProductionResult{
          .code = StrCat(left.code, "  and $", (*right_value - 1), ", ",
                         left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code =
              StrCat(left.code, right.code, "  mov ", left.result(), ", %rax\n",
                     "  xor %rdx, %rdx\n", "  idiv ", right.result(), "\n",
                     "  mov %rdx, ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code =
              StrCat(right.code, left.code, "  mov ", left.result(), ", %rax\n",
                     "  xor %rdx, %rdx\n", "  idiv ", right.result(), "\n",
                     "  mov %rdx, ", right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  mov ", result, ", %rax\n",
                     "  xor %rdx, %rdx\n", "  idiv ", right.result(), "\n",
                     "  mov %rdx, ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  ProductionResult Produce(const ir::LessThan& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  cmp ", right.result(), ", ",
                         left.result(), "\n", "  setl ",
                         ByteName(left.result()), "\n", "  movzx ",
                         ByteName(left.result()), ", ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      const int registers_used =
          std::max(left.registers_used, right.registers_used) + 1;
      EnsureEnoughRegisters(registers_used);
      const Register result = kRegisterOrder[registers_used - 1];
      return ProductionResult{
          .code = StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                         right.code, "  cmp ", right.result(), ", ", result,
                         "\n", "  setl ", ByteName(result), "\n", "  movzx ",
                         ByteName(result), ", ", result, "\n"),
          .registers_used = registers_used};
    }
  }

  ProductionResult Produce(const ir::LessOrEqual& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  cmp ", right.result(), ", ",
                         left.result(), "\n", "  setle ",
                         ByteName(left.result()), "\n", "  movzx ",
                         ByteName(left.result()), ", ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      const int registers_used =
          std::max(left.registers_used, right.registers_used) + 1;
      EnsureEnoughRegisters(registers_used);
      const Register result = kRegisterOrder[registers_used - 1];
      return ProductionResult{
          .code = StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                         right.code, "  cmp ", right.result(), ", ", result,
                         "\n", "  setle ", ByteName(result), "\n", "  movzx ",
                         ByteName(result), ", ", result, "\n"),
          .registers_used = registers_used};
    }
  }

  ProductionResult Produce(const ir::Equal& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  cmp ", right.result(), ", ",
                         left.result(), "\n", "  sete ",
                         ByteName(left.result()), "\n", "  movzx ",
                         ByteName(left.result()), ", ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      const int registers_used =
          std::max(left.registers_used, right.registers_used) + 1;
      EnsureEnoughRegisters(registers_used);
      const Register result = kRegisterOrder[registers_used - 1];
      return ProductionResult{
          .code = StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                         right.code, "  cmp ", right.result(), ", ", result,
                         "\n", "  sete ", ByteName(result), "\n", "  movzx ",
                         ByteName(result), ", ", result, "\n"),
          .registers_used = registers_used};
    }
  }

  ProductionResult Produce(const ir::NotEqual& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  cmp ", right.result(), ", ",
                         left.result(), "\n", "  setne ",
                         ByteName(left.result()), "\n", "  movzx ",
                         ByteName(left.result()), ", ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      const int registers_used =
          std::max(left.registers_used, right.registers_used) + 1;
      EnsureEnoughRegisters(registers_used);
      const Register result = kRegisterOrder[registers_used - 1];
      return ProductionResult{
          .code = StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                         right.code, "  cmp ", right.result(), ", ", result,
                         "\n", "  setne ", ByteName(result), "\n", "  movzx ",
                         ByteName(result), ", ", result, "\n"),
          .registers_used = registers_used};
    }
  }

  ProductionResult Produce(const ir::BitwiseAnd& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  and ", right.result(), ", ",
                         left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code = StrCat(right.code, left.code, "  and ", left.result(), ", ",
                         right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  and ", right.result(), ", ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  ProductionResult Produce(const ir::BitwiseOr& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  or ", right.result(), ", ",
                         left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code = StrCat(right.code, left.code, "  or ", left.result(), ", ",
                         right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  or ", right.result(), ", ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  ProductionResult Produce(const ir::BitwiseXor& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  xor ", right.result(), ", ",
                         left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code = StrCat(right.code, left.code, "  xor ", left.result(), ", ",
                         right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code =
              StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                     right.code, "  xor ", right.result(), ", ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  ProductionResult Produce(const ir::ShiftLeft& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  mov ", right.result(),
                         ", %rcx\n", "  sal %cl, ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code =
              StrCat(right.code, left.code, "  mov ", right.result(),
                     ", %rcx\n", "  mov ", left.result(), ", ", right.result(),
                     "\n", "  sal %cl, ", right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code = StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                         right.code, "  mov ", right.result(), ", %rcx\n",
                         "  sal %cl, ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  ProductionResult Produce(const ir::ShiftRight& x) {
    const ProductionResult& left = Get(x.left);
    const ProductionResult& right = Get(x.right);
    if (left.registers_used > right.registers_used) {
      // We can compute left first, leave the result where it is, then compute
      // right without accidentally clobbering left, then store the result
      // where left is.
      return ProductionResult{
          .code = StrCat(left.code, right.code, "  mov ", right.result(),
                         ", %rcx\n", "  sar %cl, ", left.result(), "\n"),
          .registers_used = left.registers_used};
    } else if (right.registers_used > left.registers_used) {
      // We can compute right first, leave the result where it is, then compute
      // left without accidentally clobbering right, then store the result
      // where right is.
      return ProductionResult{
          .code =
              StrCat(right.code, left.code, "  mov ", right.result(),
                     ", %rcx\n", "  mov ", left.result(), ", ", right.result(),
                     "\n", "  sar %cl, ", right.result(), "\n"),
          .registers_used = right.registers_used};
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      EnsureEnoughRegisters(left.registers_used + 1);
      const Register result = kRegisterOrder[left.registers_used];
      return ProductionResult{
          .code = StrCat(left.code, "  mov ", left.result(), ", ", result, "\n",
                         right.code, "  mov ", right.result(), ", %rcx\n",
                         "  sar %cl, ", result, "\n"),
          .registers_used = left.registers_used + 1};
    }
  }

  std::map<const ir::Expression*, ProductionResult> cache_;
};

class CodeGenerator {
 public:
  CodeGenerator(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const ir::Label& x) {
    *output_ << x.value << ":\n";
  }

  void operator()(const ir::Store8& x) {
    *output_ << "  // " << x << "\n";
    ProductionResult value = ExpressionGenerator().Get(x.value);
    if (auto* local = std::get_if<ir::Local>(&x.address->value)) {
      // Directly store a byte to a local.
      *output_ << value.code << "  mov " << ByteName(value.result()) << ", "
               << (std::int64_t)local->offset << "(%rbp)\n";
      return;
    }
    if (auto* global = std::get_if<ir::Global>(&x.address->value)) {
      // Directly store a byte to a global.
      *output_ << value.code << "  mov " << ByteName(value.result()) << ", "
               << global->value << "\n";
      return;
    }
    // Compute an address and then store a byte to that address.
    ProductionResult address = ExpressionGenerator().Get(x.address);
    if (value.registers_used > address.registers_used) {
      // We can compute the value first, leave the result where it is, then
      // compute the address without accidentally clobbering the value.
      *output_ << value.code << address.code << "  movb "
               << ByteName(value.result()) << ", (" << address.result()
               << ")\n";
    } else if (address.registers_used > value.registers_used) {
      // We can compute the address first, leave the result where it is, then
      // compute the value without accidentally clobbering the address.
      *output_ << address.code << value.code << "  movb "
               << ByteName(value.result()) << ", (" << address.result()
               << ")\n";
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      const Register temp = kRegisterOrder[value.registers_used];
      *output_ << value.code << "  mov " << value.result() << ", " << temp
               << "\n"
               << address.code << "  movb " << ByteName(temp) << ", ("
               << address.result() << ")\n";
    }
  }

  void operator()(const ir::Store64& x) {
    *output_ << "  // " << x << "\n";
    ProductionResult value = ExpressionGenerator().Get(x.value);
    if (auto* local = std::get_if<ir::Local>(&x.address->value)) {
      // Directly store a value to a local.
      *output_ << value.code << "  mov " << value.result() << ", "
               << (std::int64_t)local->offset << "(%rbp)\n";
      return;
    }
    if (auto* global = std::get_if<ir::Global>(&x.address->value)) {
      // Directly store a value to a global.
      *output_ << value.code << "  mov " << value.result() << ", "
               << global->value << "\n";
      return;
    }
    // Compute an address and then store a byte to that address.
    ProductionResult address = ExpressionGenerator().Get(x.address);
    if (value.registers_used > address.registers_used) {
      // We can compute the value first, leave the result where it is, then
      // compute the address without accidentally clobbering the value.
      *output_ << value.code << address.code << "  mov " << value.result()
               << ", (" << address.result() << ")\n";
    } else if (address.registers_used > value.registers_used) {
      // We can compute the address first, leave the result where it is, then
      // compute the value without accidentally clobbering the address.
      *output_ << address.code << value.code << "  mov "
               << value.result() << ", (" << address.result()
               << ")\n";
    } else {
      // We need to use one extra slot to stow the value of left while we
      // compute the value of right.
      const Register temp = kRegisterOrder[value.registers_used];
      *output_ << value.code << "  mov " << value.result() << ", " << temp
               << "\n"
               << address.code << "  mov " << temp << ", ("
               << address.result() << ")\n";
    }
  }

  void operator()(const ir::Call& x) {
    *output_ << "  // " << x << "\n";
    for (int i = x.arguments.size() - 1; i >= 0; i--) {
      ProductionResult value = ExpressionGenerator().Get(x.arguments[i]);
      *output_ << value.code << "  push " << value.result() << "\n";
    }
    ProductionResult function_address =
        ExpressionGenerator().Get(x.function_address);
    *output_ << function_address.code << "  call *" << function_address.result()
             << "\n"
                "  add $"
             << (8 * x.arguments.size()) << ", %rsp\n";
  }

  void operator()(const ir::BeginFrame& x) {
    *output_ << "  // " << x
             << "\n"
                "  push %rbp\n"
                "  mov %rsp, %rbp\n"
                "  sub $"
             << x.size << ", %rsp\n";
  }

  void operator()(const ir::Return& x) {
    *output_ << "  // " << x
             << "\n"
                "  mov %rbp, %rsp\n"
                "  pop %rbp\n"
                "  ret\n";
  }

  void operator()(const ir::Jump& x) {
    *output_ << "  // " << x
             << "\n"
                "  jmp "
             << x.target.value << "\n";
  }

  void operator()(const ir::JumpIf& x) {
    ProductionResult condition = ExpressionGenerator().Get(x.condition);
    *output_ << "  // " << x << "\n"
             << condition.code << "  test " << condition.result() << ", "
             << condition.result()
             << "\n"
                "  jnz "
             << x.target.value << "\n";
  }

  void operator()(const ir::JumpUnless& x) {
    ProductionResult condition = ExpressionGenerator().Get(x.condition);
    *output_ << "  // " << x << "\n"
             << condition.code << "  test " << condition.result() << ", "
             << condition.result()
             << "\n"
                "  jz "
             << x.target.value << "\n";
  }

  void operator()(const ir::Sequence& x) {
    for (const auto& y : x.value) std::visit(*this, y->value);
  }

  std::ostream* output_;
};

}  // namespace

std::string Generate(const ir::Unit& unit) {
  assert(unit.main.has_value());
  std::ostringstream result;
  result << ".section .bss\n"
            "  .align 8\n";
  for (const auto& [global, size] : unit.data) {
    result << global.value << ":\n  .space " << size << '\n';
  }

  result << ".section .rodata\n"
            "  .align 8\n";
  for (const auto& [global, value] : unit.string_literals) {
    result << global.value << ":\n  .byte ";
    for (char c : value) result << (int)c << ", ";
    result << "0\n";
  }

  result << ".section .text\n"
            // function read(fd: int64, buffer: []byte, size: int64): int64;
            "read:\n"
            "  mov $0, %rax\n"
            "  mov 16(%rsp), %rdi\n"
            "  mov 24(%rsp), %rsi\n"
            "  mov 32(%rsp), %rdx\n"
            "  syscall\n"
            "  mov 8(%rsp), %rdi\n"
            "  mov %rax, (%rdi)\n"
            "  ret\n"
            // function write(fd: int64, buffer: []byte, size: int64): int64;
            "write:\n"
            "  mov $1, %rax\n"
            "  mov 16(%rsp), %rdi\n"
            "  mov 24(%rsp), %rsi\n"
            "  mov 32(%rsp), %rdx\n"
            "  syscall\n"
            "  mov 8(%rsp), %rdi\n"
            "  mov %rax, (%rdi)\n"
            "  ret\n"
            // function exit(code: int64): void;
            "exit:\n"
            "  mov $60, %rax\n"
            "  mov 8(%rsp), %rdi\n"
            "  syscall\n"
            // function copy(dest: []byte, source: []byte, size: int64): void;
            "copy:\n"
            "  mov 8(%rsp), %rdi\n"
            "  mov 16(%rsp), %rsi\n"
            "  mov 24(%rsp), %rax\n"
            "  add %rsi, %rax\n"
            "  jmp .Lcopy_loop_condition\n"
            ".Lcopy_loop:\n"
            "  movb (%rsi), %cl\n"
            "  movb %cl, (%rdi)\n"
            "  inc %rdi\n"
            "  inc %rsi\n"
            ".Lcopy_loop_condition:\n"
            "  cmp %rax, %rsi\n"
            "  jl .Lcopy_loop\n"
            "  ret\n"
            ".global _start\n"
            "_start:\n"
            "  sub $8, %rsp\n"
            "  push %rsp\n"
            "  call "
         << unit.main->value
         << "\n"
            "  add $8, %rsp\n"
            "  // exit\n"
            "  pop %rdi\n"
            "  mov $60, %rax\n"
            "  syscall\n";
  std::visit(CodeGenerator(result), unit.code->value);
  return result.str();
}

}  // namespace aoc2021
