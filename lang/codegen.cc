#include "codegen.h"

#include <cassert>
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

class ExpressionGenerator {
 public:
  ExpressionGenerator(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const ir::Label& x) {
    *output_ << "  // " << x << "\n  push $" << x.value
             << '\n';
  }

  void operator()(const ir::Global& x) {
    *output_ << "  // " << x << "\n  push $" << x.value << '\n';
  }

  void operator()(const ir::Local& x) {
    *output_ << "  // " << x << "\n  lea " << (8 * (std::int64_t)x.offset)
             << "(%rbp), %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::Load64& x) {
    std::visit(*this, x.address->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  push (%rax)\n";
  }

  void operator()(const ir::IntegerLiteral& x) {
    *output_ << "  // " << x << "\n  push $" << x.value << '\n';
  }

  void operator()(const ir::Negate& x) {
    std::visit(*this, x.inner->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  neg %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::LogicalNot& x) {
    std::visit(*this, x.inner->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  test %rax, %rax\n"
                "  sete %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::BitwiseNot& x) {
    std::visit(*this, x.inner->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  not %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::Add& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  add %rax, (%rsp)\n";
  }

  void operator()(const ir::Subtract& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  sub %rax, (%rsp)\n";
  }

  void operator()(const ir::Multiply& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  imul (%rsp), %rax\n"
                "  mov %rax, (%rsp)\n";
  }

  void operator()(const ir::Divide& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rbx\n"
                "  pop %rax\n"
                "  xor %rdx, %rdx\n"
                "  idiv %rbx\n"
                "  push %rax\n";
  }

  void operator()(const ir::Modulo& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rbx\n"
                "  pop %rax\n"
                "  xor %rdx, %rdx\n"
                "  idiv %rbx\n"
                "  push %rdx\n";
  }

  void operator()(const ir::LessThan& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  cmp %rax, (%rsp)\n"
                "  setl %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::LessOrEqual& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  cmp %rax, (%rsp)\n"
                "  setle %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::Equal& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  test %rax, (%rsp)\n"
                "  sete %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::NotEqual& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  cmp %rax, (%rsp)\n"
                "  setne %bl\n"
                "  push %rbx\n";
  }

  void operator()(const ir::BitwiseAnd& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  and %rax, (%rsp)\n";
  }

  void operator()(const ir::BitwiseOr& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  or %rax, (%rsp)\n";
  }

  void operator()(const ir::BitwiseXor& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rax, (%rsp)\n";
  }

  void operator()(const ir::ShiftLeft& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rcx\n"
                "  salq %cl, (%rsp)\n";
  }

  void operator()(const ir::ShiftRight& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rcx\n"
                "  sarq %cl, (%rsp)\n";
  }

 private:
  std::ostream* output_;
};

class CodeGenerator {
 public:
  CodeGenerator(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const ir::Label& x) {
    *output_ << x.value << ":\n";
  }

  void operator()(const ir::Store64& x) {
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.value->value);
    std::visit(generator, x.address->value);
    *output_ << "  // ir::Store64\n"
                "  pop %rbx\n"
                "  pop (%rbx)\n";
  }

  void operator()(const ir::StoreCall64& x) {
    ExpressionGenerator generator(*output_);
    for (int i = x.arguments.size() - 1; i >= 0; i--) {
      std::visit(generator, x.arguments[i]->value);
    }
    std::visit(generator, x.result_address->value);
    std::visit(generator, x.function_address->value);
    *output_ << "  // ir::StoreCall64\n"
                "  pop %rax\n"
                "  call *%rax\n"
                "  add $" << (8 * (x.arguments.size() + 1)) << ", %rsp\n";
  }

  void operator()(const ir::BeginFrame& x) {
    *output_ << "  // ir::BeginFrame\n"
                "  push %rbp\n"
                "  mov %rsp, %rbp\n"
                "  sub $"
             << (8 * x.size) << ", %rsp\n";
  }

  void operator()(const ir::Return& x) {
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.value->value);
    *output_ << "  // ir::Return\n"
                "  mov 16(%rbp), %rax\n"
                "  pop (%rax)\n"
                "  mov %rbp, %rsp\n"
                "  pop %rbp\n"
                "  ret\n";
  }

  void operator()(const ir::Jump& x) {
    *output_ << "  jmp " << x.target.value << "\n";
  }

  void operator()(const ir::JumpIf& x) {
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.condition->value);
    *output_ << "  // ir::JumpIf\n"
                "  pop %rax\n"
                "  test %rax, %rax\n"
                "  jnz " << x.target.value << "\n";
  }

  void operator()(const ir::JumpUnless& x) {
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.condition->value);
    *output_ << "  // ir::JumpUnless\n"
                "  pop %rax\n"
                "  test %rax, %rax\n"
                "  jz " << x.target.value << "\n";
  }

  void operator()(const ir::Sequence& x) {
    for (const auto& part : x.value) std::visit(*this, part->value);
  }

 private:
  std::ostream* output_;
};

}  // namespace

std::string Generate(const ir::Unit& unit) {
  assert(unit.main.has_value());
  std::ostringstream result;
  result << ".section .bss\n"
            "  .align 8\n";
  for (const auto& [global, size] : unit.data) {
    result << global.value << ":\n  .space " << (8 * size) << '\n';
  }

  result << ".section .text\n"
            "read:\n"
            "  mov $0, %rax\n"
            "  mov 16(%rsp), %rdi\n"
            "  mov 24(%rsp), %rsi\n"
            "  mov 32(%rsp), %rdx\n"
            "  syscall\n"
            "  ret\n"
            "write:\n"
            "  mov $1, %rax\n"
            "  mov 16(%rsp), %rdi\n"
            "  mov 24(%rsp), %rsi\n"
            "  mov 32(%rsp), %rdx\n"
            "  syscall\n"
            "  ret\n"
            "exit:\n"
            "  mov $60, %rax\n"
            "  mov 16(%rsp), %rdi\n"
            "  syscall\n"
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
