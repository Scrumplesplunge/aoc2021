#include "codegen.h"

#include <sstream>

namespace aoc2021 {
namespace {

struct Label { ir::Label value; };

std::ostream& operator<<(std::ostream& output, const Label& l) {
  return output << "label" << (std::int64_t)l.value;
}

class CodeGenerator : public ir::CodeVisitor<void> {
 public:
  CodeGenerator(std::ostream& output) noexcept : output_(&output) {}

  void operator()(const ir::Label& x) override {
    *output_ << Label(x) << ":\n";
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
    *output_ << "  jmp " << Label(x.target) << "\n";
  }

  void operator()(const ir::JumpIf& x) override {
    *output_ << "  <JUMP IF CONDITION> " << Label(x.target) << "\n";
  }

  void operator()(const ir::JumpUnless& x) override {
    *output_ << "  <JUMP UNLESS CONDITION> " << Label(x.target) << "\n";
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
