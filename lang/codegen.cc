#include "codegen.h"

#include <sstream>

namespace aoc2021 {
namespace {

class CodeGenerator : public ir::CodeVisitor<void> {
 public:
  CodeGenerator(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const ir::Label& x) override {
    *output_ << x.value << ":\n";
  }

  void operator()(const ir::Store64&) override {
    *output_ << "  mov <VALUE>, <ADDRESS>\n";
  }

  void operator()(const ir::StoreCall64&) override {
    *output_ << "  call <TODO>\n"
                "  mov <RETURN VALUE>, <TODO>\n";
  }

  void operator()(const ir::AdjustStack& x) override {
    *output_ << "  add " << (8 * x.delta) << ", %rsp\n";
  }

  void operator()(const ir::Return&) override {
    *output_ << "  mov <RETURN VALUE>, <RETURN SLOT>\n"
                "  mov %rbp, %rsp\n"
                "  pop %rbp\n"
                "  ret\n";
  }

  void operator()(const ir::Jump& x) override {
    *output_ << "  jmp " << x.target.value << "\n";
  }

  void operator()(const ir::JumpIf& x) override {
    *output_ << "  <JUMP IF CONDITION> " << x.target.value << "\n";
  }

  void operator()(const ir::JumpUnless& x) override {
    *output_ << "  <JUMP UNLESS CONDITION> " << x.target.value << "\n";
  }

  void operator()(const ir::Sequence& x) override {
    for (const auto& part : x.value) part.Visit(*this);
  }

 private:
  std::ostream* output_;
};

}  // namespace

std::string Generate(ir::AnyCode code) {
  std::ostringstream result;
  CodeGenerator generator(result);
  code.Visit(generator);
  return result.str();
}

}  // namespace aoc2021
