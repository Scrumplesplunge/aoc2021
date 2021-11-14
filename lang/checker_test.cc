#include "checker.h"

#include "parser.h"
#include "test.h"

#include <deque>

namespace aoc2021 {
namespace {

struct CheckerTest : Test {
  std::vector<ast::Statement> Program(std::string_view value) {
    auto& source = sources.emplace_back("test_input.aoc", value);
    return Parser(source).ParseProgram();
  }

  // An AST contains location references that point back at the source, so the
  // underlying source for any expression or statement must outlive the use of
  // that AST.
  std::deque<Source> sources;
};

TEST_F(CheckerTest, Empty) {
  const ir::Unit unit = Check(Program(""));
  EXPECT_FALSE(unit.main.has_value());
  EXPECT_EQ(unit.code, ir::Sequence({}));
}

TEST_F(CheckerTest, Variables) {
  const ir::Unit unit = Check(Program(R"(
    var x;
    var y[10];
    var main;
  )"));
  EXPECT_FALSE(unit.main.has_value());
  EXPECT_EQ(unit.code, ir::Sequence({}));
}

TEST_F(CheckerTest, Redeclaration) {
  EXPECT_ERROR(Check(Program("var x; var x[10];")),
               "error: redeclaration of 'x'");
  EXPECT_ERROR(Check(Program("var x; function x() {}")),
               "error: redeclaration of 'x'");
}

TEST_F(CheckerTest, NotAnLvalue) {
  EXPECT_ERROR(Check(Program("function f() { 1 = 1; }")),
               "is not an lvalue");
  EXPECT_ERROR(Check(Program("function f() { f = 1; }")),
               "is not an lvalue");
  EXPECT_ERROR(Check(Program("function f() { var x[1]; x = 1; }")),
               "is not an lvalue");
}

TEST_F(CheckerTest, Undeclared) {
  EXPECT_ERROR(Check(Program("function f() { g(); }")),
               "use of undeclared name 'g'");
  EXPECT_ERROR(Check(Program("function f() { g = 1; }")),
               "use of undeclared name 'g'");
}

TEST_F(CheckerTest, ArraySizeConstant) {
  EXPECT_ERROR(Check(Program("var x; var y[x];")),
               "array size must be a constant expression");
  EXPECT_ERROR(Check(Program("function f(x) { var y[x]; }")),
               "array size must be a constant expression");
}

TEST_F(CheckerTest, BreakOutsideBreakable) {
  EXPECT_ERROR(Check(Program("function f() { break; }")),
               "break statement outside of a breakable context");
}

TEST_F(CheckerTest, ContinueOutsideLoop) {
  EXPECT_ERROR(Check(Program("function f() { continue; }")),
               "continue statement outside of a loop");
}

TEST_F(CheckerTest, NestedFunctions) {
  EXPECT_ERROR(Check(Program("function f() { function g() {} }")),
               "nested function definitions are forbidden");
}

TEST_F(CheckerTest, ForbiddenAtModuleScope) {
  EXPECT_ERROR(Check(Program("var x; x = 1;")), "forbidden at module scope");
  EXPECT_ERROR(Check(Program("if 1 {}")), "forbidden at module scope");
  EXPECT_ERROR(Check(Program("while 1 {}")), "forbidden at module scope");
  EXPECT_ERROR(Check(Program("1 + 2;")), "forbidden at module scope");
  EXPECT_ERROR(Check(Program("return;")),
               "return statement outside of a function");
  EXPECT_ERROR(Check(Program("break;")),
               "break statement outside of a breakable context");
  EXPECT_ERROR(Check(Program("continue;")),
               "continue statement outside of a loop");
}

}  // namespace
}  // namespace aoc2021
