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
    var x: int64;
    var y: [10]int64;
    var main: int64;
  )"));
  EXPECT_FALSE(unit.main.has_value());
  EXPECT_EQ(unit.code, ir::Sequence({}));
}

TEST_F(CheckerTest, Redeclaration) {
  EXPECT_ERROR(Check(Program("var x: int64; var x: [10]int64;")),
               "error: redeclaration of 'x'");
  EXPECT_ERROR(Check(Program("var x: int64; function x(): void {}")),
               "error: redeclaration of 'x'");
}

TEST_F(CheckerTest, NotAnLvalue) {
  EXPECT_ERROR(Check(Program("function f(): void { 1 = 1; }")),
               "not an lvalue");
  EXPECT_ERROR(Check(Program("function f(): void { f = 1; }")),
               "not an lvalue");
  EXPECT_ERROR(Check(Program("function f(): void { &1; }")), "not an lvalue");
}

TEST_F(CheckerTest, Undeclared) {
  EXPECT_ERROR(Check(Program("function f(): void { g(); }")),
               "use of undeclared name 'g'");
  EXPECT_ERROR(Check(Program("function f(): void { g = 1; }")),
               "use of undeclared name 'g'");
}

TEST_F(CheckerTest, ArraySizeConstant) {
  EXPECT_ERROR(Check(Program("var x: int64; var y: [x]int64;")),
               "array size must be a constant expression");
  EXPECT_ERROR(
      Check(Program("function f(x: int64): void { var y: [x]int64; }")),
      "array size must be a constant expression");
}

TEST_F(CheckerTest, BreakOutsideBreakable) {
  EXPECT_ERROR(Check(Program("function f(): void { break; }")),
               "break statement outside of a breakable context");
}

TEST_F(CheckerTest, ContinueOutsideLoop) {
  EXPECT_ERROR(Check(Program("function f(): void { continue; }")),
               "continue statement outside of a loop");
}

TEST_F(CheckerTest, NestedFunctions) {
  EXPECT_ERROR(Check(Program("function f(): void { function g(): void {} }")),
               "nested function definitions are forbidden");
}

TEST_F(CheckerTest, ForbiddenAtModuleScope) {
  EXPECT_ERROR(Check(Program("var x: int64; x = 1;")),
               "forbidden at module scope");
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

TEST_F(CheckerTest, ValueUsedInTypeContext) {
  EXPECT_ERROR(Check(Program("var x: int64; var y: x;")),
               "value used in type context");
  EXPECT_ERROR(Check(Program("var x: int64; function f(a: x): void {}")),
               "value used in type context");
  EXPECT_ERROR(Check(Program("var x: int64; function f(): x {}")),
               "value used in type context");
}

TEST_F(CheckerTest, TypeUsedInValueContext) {
  EXPECT_ERROR(Check(Program("function f(): void { int64 = 1; }")),
               "type used in a value context");
}

TEST_F(CheckerTest, NotAnInteger) {
  EXPECT_ERROR(Check(Program("function f(x: *int64): void { -x; }")),
               "not an integer");
  EXPECT_ERROR(Check(Program("function f(x: *int64): void { x + 1; }")),
               "not an integer");
}

TEST_F(CheckerTest, UsingNonFunctionAsFunction) {
  EXPECT_ERROR(Check(Program("var x: int64; function f(): void { x(); }")),
               "trying to use Primitive::kInt64 as a function");
}

TEST_F(CheckerTest, WrongNumberOfArguments) {
  EXPECT_ERROR(Check(Program("function f(): void {}\n"
                             "function g(): void { f(1); }")),
               "wrong number of arguments for function");
}

TEST_F(CheckerTest, WrongTypeForParameter) {
  EXPECT_ERROR(Check(Program("function f(x: *int64): void {}\n"
                             "function g(): void { f(1); }")),
               "cannot implicitly convert Primitive::kInt64 to "
               "Pointer(Primitive::kInt64)");
}

TEST_F(CheckerTest, ArrayIndexMustBeInteger) {
  EXPECT_ERROR(
      Check(Program("var x: [10]int64; function f(p: *int64): void { x[p]; }")),
      "array index must be an integer");
}

TEST_F(CheckerTest, CannotIndexType) {
  EXPECT_ERROR(Check(Program("var x: int64; function f(): void { x[1]; }")),
               "cannot index a value of type");
}

TEST_F(CheckerTest, CannotDereference) {
  EXPECT_ERROR(Check(Program("var x: int64; function f(): void { *x; }")),
               "cannot dereference");
}

TEST_F(CheckerTest, IncompatibleTypes) {
  EXPECT_ERROR(
      Check(Program("function f(x: int64, y: *int64): void { x == y; }")),
      "incompatible types for equality comparison");
}

TEST_F(CheckerTest, TernaryDifferentTypes) {
  EXPECT_ERROR(
      Check(Program("function f(x: int64, y: *int64): void { x ? x : y; }")),
      "ternary expression branches yield different types");
}

TEST_F(CheckerTest, AssignmentTypeMismatch) {
  EXPECT_ERROR(
      Check(Program("function f(x: int64, y: *int64): void { x = y; }")),
      "cannot implicitly convert Pointer(Primitive::kInt64) to "
      "Primitive::kInt64");
}

TEST_F(CheckerTest, ArrayAssignment) {
  Check(Program(R"(
    function main(): void {
      var x: [1]int64;
      x[0] = 1;
    }
  )"));
}

TEST_F(CheckerTest, ArrayPointerToSlice) {
  Check(Program(R"(
    function main(): void {
      var x: [1]int64;
      var y: []int64;
      y = &x;
    }
  )"));
}

}  // namespace
}  // namespace aoc2021
