#include "codegen.h"

#include <sstream>
#include <variant>

namespace aoc2021 {
namespace {

// enum class Register {
//   kRax,
//   kRbx,
//   kRcx,
//   kRdx,
//   kRdi,
//   kRsi,
//   kRbp,
//   kRsp,
// };
// 
// std::ostream& operator<<(std::ostream& output, Register r) {
//   switch (r) {
//     case Register::kRax: return output << "%rax";
//     case Register::kRbx: return output << "%rbx";
//     case Register::kRcx: return output << "%rcx";
//     case Register::kRdx: return output << "%rdx";
//     case Register::kRdi: return output << "%rdi";
//     case Register::kRsi: return output << "%rsi";
//     case Register::kRbp: return output << "%rbp";
//     case Register::kRsp: return output << "%rsp";
//   }
//   return output << "?";
// }
// 
// struct DirectValue {
//   std::variant<std::int64_t, std::string> value;
// };
// 
// std::ostream& operator<<(std::ostream& output, const DirectValue& x) {
//   std::visit([&](auto& y) { output << y; }, x.value);
//   return output;
// }
// 
// struct Literal { DirectValue value; };
// struct Direct { DirectValue address; };
// struct Indirect { Register base; std::int64_t offset; };
// 
// std::ostream& operator<<(std::ostream& output, const Literal& x) {
//   return output << '$' << x;
// }
// 
// std::ostream& operator<<(std::ostream& output, const Direct& x) {
//   return output << x.address;
// }
// 
// std::ostream& operator<<(std::ostream& output, const Indirect& x) {
//   if (x.offset) {
//     return output << x.offset << '(' << x.base << ')';
//   } else {
//     return output << '(' << x.base << ')';
//   }
// }
// 
// struct Operand {
//   std::variant<Literal, Register, Direct, Indirect> value;
// };
// 
// std::ostream& operator<<(std::ostream& output, const Operand& x) {
//   std::visit([&](auto& y) { output << y; }, x.value);
//   return output;
// }

class ExpressionGenerator : public ir::ExpressionVisitor<void> {
 public:
  ExpressionGenerator(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const ir::Label& x) override {
    *output_ << "  // ir::Label\n"
                "  push $"
             << x.value << '\n';
  }

  void operator()(const ir::Global& x) override {
    *output_ << "  // ir::Global\n"
                "  push $"
             << x.value << '\n';
  }

  void operator()(const ir::Local& x) override {
    *output_ << "  // ir::Local\n"
                "  lea "
             << (8 * (std::int64_t)x.offset)
             << "(%rbp), %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::Load64& x) override {
    x.address.Visit(*this);
    *output_ << "  // ir::Load64\n"
                "  pop %rax\n"
                "  push (%rax)\n";
  }

  void operator()(const ir::IntegerLiteral& x) override {
    *output_ << "  // ir::IntegerLiteral\n"
                "  push $" << x.value << '\n';
  }

  void operator()(const ir::Negate& x) override {
    x.inner.Visit(*this);
    *output_ << "  // ir::Negate\n"
                "  pop %rax\n"
                "  neg %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::LogicalNot& x) override {
    x.inner.Visit(*this);
    *output_ << "  // ir::LogicalNot\n"
                "  pop %rax\n"
                "  test %rax, %rax\n"
                "  xor %rbx, %rbx\n"
                "  sete %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::BitwiseNot& x) override {
    x.inner.Visit(*this);
    *output_ << "  // ir::BitwiseNot\n"
                "  pop %rax\n"
                "  not %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::Add& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::Add\n"
                "  pop %rax\n"
                "  add %rax, (%rsp)\n";
  }

  void operator()(const ir::Subtract& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::Subtract\n"
                "  pop %rax\n"
                "  sub %rax, (%rsp)\n";
  }

  void operator()(const ir::Multiply& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::Multiply\n"
                "  pop %rax\n"
                "  imul (%rsp), %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::Divide& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::Divide\n"
                "  pop %rbx\n"
                "  pop %rax\n"
                "  xor %rdx, %rdx\n"
                "  idiv %rbx\n"
                "  push %rax\n";
  }

  void operator()(const ir::Modulo& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::Modulo\n"
                "  pop %rbx\n"
                "  pop %rax\n"
                "  xor %rdx, %rdx\n"
                "  idiv %rbx\n"
                "  push %rdx\n";
  }

  void operator()(const ir::LessThan& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::LessThan\n"
                "  pop %rax\n"
                "  cmp %rax, (%rsp)\n"
                "  xor %rbx, %rbx\n"
                "  setl %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::LessOrEqual& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::LessOrEqual\n"
                "  pop %rax\n"
                "  cmp %rax, (%rsp)\n"
                "  xor %rbx, %rbx\n"
                "  setle %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::Equal& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::Equal\n"
                "  pop %rax\n"
                "  test %rax, (%rsp)\n"
                "  xor %rbx, %rbx\n"
                "  sete %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::NotEqual& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::NotEqual\n"
                "  pop %rax\n"
                "  cmp %rax, (%rsp)\n"
                "  xor %rbx, %rbx\n"
                "  setne %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::BitwiseAnd& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::BitwiseAnd\n"
                "  pop %rax\n"
                "  and %rax, (%rsp)\n";
  }

  void operator()(const ir::BitwiseOr& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::BitwiseOr\n"
                "  pop %rax\n"
                "  or %rax, (%rsp)\n";
  }

  void operator()(const ir::BitwiseXor& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::BitwiseXor\n"
                "  pop %rax\n"
                "  xor %rax, (%rsp)\n";
  }

  void operator()(const ir::ShiftLeft& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::BitwiseXor\n"
                "  pop %rax\n"
                "  sal %al, (%rsp)\n";
  }

  void operator()(const ir::ShiftRight& x) override {
    x.left.Visit(*this);
    x.right.Visit(*this);
    *output_ << "  // ir::BitwiseXor\n"
                "  pop %rax\n"
                "  sar %al, (%rsp)\n";
  }

 private:
  std::ostream* output_;
};

class CodeGenerator : public ir::CodeVisitor<void> {
 public:
  CodeGenerator(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const ir::Label& x) override {
    *output_ << x.value << ":\n";
  }

  void operator()(const ir::Store64& x) override {
    ExpressionGenerator generator(*output_);
    x.value.Visit(generator);
    x.address.Visit(generator);
    *output_ << "  // ir::Store64\n"
                "  pop %rbx\n"
                "  pop (%rbx)\n";
  }

  void operator()(const ir::StoreCall64& x) override {
    ExpressionGenerator generator(*output_);
    for (int i = x.arguments.size() - 1; i >= 0; i--) {
      x.arguments[i].Visit(generator);
    }
    x.result_address.Visit(generator);
    x.function_address.Visit(generator);
    *output_ << "  // ir::StoreCall64\n"
                "  pop %rax\n"
                "  call *%rax\n"
                "  add $" << (8 * (x.arguments.size() + 1)) << ", %rsp\n";
  }

  void operator()(const ir::BeginFrame& x) override {
    *output_ << "  // ir::BeginFrame\n"
                "  push %rbp\n"
                "  mov %rsp, %rbp\n"
                "  sub $"
             << (8 * x.size) << ", %rsp\n";
  }

  void operator()(const ir::Return& x) override {
    ExpressionGenerator generator(*output_);
    x.value.Visit(generator);
    *output_ << "  // ir::Return\n"
                "  mov 16(%rbp), %rax\n"
                "  pop (%rax)\n"
                "  mov %rbp, %rsp\n"
                "  pop %rbp\n"
                "  ret\n";
  }

  void operator()(const ir::Jump& x) override {
    *output_ << "  jmp " << x.target.value << "\n";
  }

  void operator()(const ir::JumpIf& x) override {
    *output_ << "  // ir::JumpIf\n"
                "  pop %rax\n"
                "  test %rax, %rax\n"
                "  jnz " << x.target.value << "\n";
  }

  void operator()(const ir::JumpUnless& x) override {
    *output_ << "  // ir::JumpUnless\n"
                "  pop %rax\n"
                "  test %rax, %rax\n"
                "  jz " << x.target.value << "\n";
  }

  void operator()(const ir::Sequence& x) override {
    for (const auto& part : x.value) part.Visit(*this);
  }

 private:
  std::ostream* output_;
};

constexpr char kPrelude[] = R"(
.section .text
.global _start
_start:
  sub $8, %rsp
  push %rsp
  call label_function_1
  add $8, %rsp
  // exit
  pop %rdi
  mov $60, %rax
  syscall
)";

}  // namespace

std::string Generate(ir::AnyCode code) {
  std::ostringstream result;
  result << kPrelude;
  CodeGenerator generator(result);
  code.Visit(generator);
  return result.str();
}

}  // namespace aoc2021
