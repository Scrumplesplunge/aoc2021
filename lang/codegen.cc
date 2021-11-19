#include "codegen.h"

#include <cassert>
#include <iomanip>
#include <sstream>
#include <variant>

namespace aoc2021 {
namespace {

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
    *output_ << "  // " << x << "\n  lea " << (std::int64_t)x.offset
             << "(%rbp), %rax\n"
                "  push %rax\n";
  }

  void operator()(const ir::Load8& x) {
    std::visit(*this, x.address->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  movzxb (%rax), %rax\n"
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
                "  mov %rbx, (%rsp)\n";
  }

  void operator()(const ir::LessOrEqual& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  cmp %rax, (%rsp)\n"
                "  setle %bl\n"
                "  mov %rbx, (%rsp)\n";
  }

  void operator()(const ir::Equal& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  cmp %rax, (%rsp)\n"
                "  sete %bl\n"
                "  mov %rbx, (%rsp)\n";
  }

  void operator()(const ir::NotEqual& x) {
    std::visit(*this, x.left->value);
    std::visit(*this, x.right->value);
    *output_ << "  // " << x
             << "\n  pop %rax\n"
                "  xor %rbx, %rbx\n"
                "  cmp %rax, (%rsp)\n"
                "  setne %bl\n"
                "  mov %rbx, (%rsp)\n";
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

  void operator()(const ir::Store8& x) {
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.value->value);
    std::visit(generator, x.address->value);
    *output_ << "  // " << x
             << "\n"
                "  pop %rbx\n"
                "  pop %rax\n"
                "  movb %al, (%rbx)\n";
  }

  void operator()(const ir::Store64& x) {
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.value->value);
    std::visit(generator, x.address->value);
    *output_ << "  // " << x
             << "\n"
                "  pop %rbx\n"
                "  pop (%rbx)\n";
  }

  void operator()(const ir::Call& x) {
    ExpressionGenerator generator(*output_);
    for (int i = x.arguments.size() - 1; i >= 0; i--) {
      std::visit(generator, x.arguments[i]->value);
    }
    std::visit(generator, x.function_address->value);
    *output_ << "  // " << x
             << "\n"
                "  pop %rax\n"
                "  call *%rax\n"
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
    ExpressionGenerator generator(*output_);
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
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.condition->value);
    *output_ << "  // " << x
             << "\n"
                "  pop %rax\n"
                "  test %rax, %rax\n"
                "  jnz "
             << x.target.value << "\n";
  }

  void operator()(const ir::JumpUnless& x) {
    ExpressionGenerator generator(*output_);
    std::visit(generator, x.condition->value);
    *output_ << "  // " << x
             << "\n"
                "  pop %rax\n"
                "  test %rax, %rax\n"
                "  jz "
             << x.target.value << "\n";
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
