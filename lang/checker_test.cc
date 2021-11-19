#include "checker.h"

#include <deque>

#include "parser.h"
#include "test.h"

namespace aoc2021 {
namespace {

struct CheckerTest : Test {
  CheckerTest()
      : checker() {}

  void WithSource(std::string_view name, std::string_view source) {
    sources[std::filesystem::absolute(name)] = Source(name, source);
  }

  ir::Unit Check(std::string_view value) {
    WithSource("test_input.aoc", value);
    Checker checker([&](auto& path) {
      return sources.at(std::filesystem::absolute(path));
    });
    return checker.Check("test_input.aoc");
  }

  // An AST contains location references that point back at the source, so the
  // underlying source for any expression or statement must outlive the use of
  // that AST.
  std::map<std::filesystem::path, Source> sources;
  Checker checker;
};

TEST_F(CheckerTest, Empty) {
  const ir::Unit unit = Check("");
  EXPECT_FALSE(unit.main.has_value());
  EXPECT_EQ(unit.code, ir::Sequence({}));
}

TEST_F(CheckerTest, Variables) {
  const ir::Unit unit = Check(R"(
    var x: int64;
    var y: [10]int64;
    var main: int64;
  )");
  EXPECT_FALSE(unit.main.has_value());
  EXPECT_EQ(unit.code, ir::Sequence({}));
}

TEST_F(CheckerTest, Redeclaration) {
  EXPECT_ERROR(Check("var x: int64; var x: [10]int64;"),
               "error: redeclaration of 'x'");
  EXPECT_ERROR(Check("var x: int64; function x(): void {}"),
               "error: redeclaration of 'x'");
}

TEST_F(CheckerTest, NotAnLvalue) {
  EXPECT_ERROR(Check("function f(): void { 1 = 1; }"), "not an lvalue");
  EXPECT_ERROR(Check("function f(): void { f = 1; }"), "not an lvalue");
  EXPECT_ERROR(Check("function f(): void { &1; }"), "not an lvalue");
}

TEST_F(CheckerTest, Undeclared) {
  EXPECT_ERROR(Check("function f(): void { g(); }"),
               "use of undeclared name 'g'");
  EXPECT_ERROR(Check("function f(): void { g = 1; }"),
               "use of undeclared name 'g'");
}

TEST_F(CheckerTest, ArraySizeConstant) {
  EXPECT_ERROR(Check("var x: int64; var y: [x]int64;"),
               "array size must be a constant expression");
  EXPECT_ERROR(Check("function f(x: int64): void { var y: [x]int64; }"),
               "array size must be a constant expression");
}

TEST_F(CheckerTest, BreakOutsideBreakable) {
  EXPECT_ERROR(Check("function f(): void { break; }"),
               "break statement outside of a breakable context");
}

TEST_F(CheckerTest, ContinueOutsideLoop) {
  EXPECT_ERROR(Check("function f(): void { continue; }"),
               "continue statement outside of a loop");
}

TEST_F(CheckerTest, NestedFunctions) {
  EXPECT_ERROR(Check("function f(): void { function g(): void {} }"),
               "nested function definitions are forbidden");
}

TEST_F(CheckerTest, ForbiddenAtModuleScope) {
  EXPECT_ERROR(Check("var x: int64; x = 1;"), "forbidden at module scope");
  EXPECT_ERROR(Check("if 1 {}"), "forbidden at module scope");
  EXPECT_ERROR(Check("while 1 {}"), "forbidden at module scope");
  EXPECT_ERROR(Check("1 + 2;"), "forbidden at module scope");
  EXPECT_ERROR(Check("return;"), "return statement outside of a function");
  EXPECT_ERROR(Check("break;"),
               "break statement outside of a breakable context");
  EXPECT_ERROR(Check("continue;"), "continue statement outside of a loop");
}

TEST_F(CheckerTest, ValueUsedInTypeContext) {
  EXPECT_ERROR(Check("var x: int64; var y: x;"),
               "value used in type context");
  EXPECT_ERROR(Check("var x: int64; function f(a: x): void {}"),
               "value used in type context");
  EXPECT_ERROR(Check("var x: int64; function f(): x {}"),
               "value used in type context");
}

TEST_F(CheckerTest, TypeUsedInValueContext) {
  EXPECT_ERROR(Check("function f(): void { int64 = 1; }"),
               "type used in a value context");
}

// TODO: Split and/or rename this test.
TEST_F(CheckerTest, NotAnInteger) {
  EXPECT_ERROR(Check("function f(x: *int64): void { -x; }"),
               "not an integer");
  EXPECT_ERROR(Check("function f(x: *int64): void { x + 1; }"),
               "cannot add Pointer(Scalar::kInt64) and Scalar::kInt64");
}

TEST_F(CheckerTest, UsingNonFunctionAsFunction) {
  EXPECT_ERROR(Check("var x: int64; function f(): void { x(); }"),
               "trying to use Scalar::kInt64 as a function");
}

TEST_F(CheckerTest, WrongNumberOfArguments) {
  EXPECT_ERROR(Check("function f(): void {}\n"
                      "function g(): void { f(1); }"),
               "wrong number of arguments for function");
}

TEST_F(CheckerTest, WrongTypeForParameter) {
  EXPECT_ERROR(Check("function f(x: *int64): void {}\n"
                      "function g(): void { f(1); }"),
               "cannot implicitly convert Scalar::kInt64 to "
               "Pointer(Scalar::kInt64)");
}

TEST_F(CheckerTest, ArrayIndexMustBeInteger) {
  EXPECT_ERROR(
      Check("var x: [10]int64; function f(p: *int64): void { x[p]; }"),
      "not an integer");
}

TEST_F(CheckerTest, CannotIndexType) {
  EXPECT_ERROR(Check("var x: int64; function f(): void { x[1]; }"),
               "cannot index a value of type");
}

TEST_F(CheckerTest, CannotDereference) {
  EXPECT_ERROR(Check("var x: int64; function f(): void { *x; }"),
               "cannot dereference");
}

TEST_F(CheckerTest, IncompatibleTypes) {
  EXPECT_ERROR(Check("function f(x: int64, y: *int64): void { x == y; }"),
               "incompatible types for equality comparison");
}

TEST_F(CheckerTest, TernaryDifferentTypes) {
  EXPECT_ERROR(Check("function f(x: int64, y: *int64): void { x ? x : y; }"),
               "ternary expression branches yield different types");
}

TEST_F(CheckerTest, AssignmentTypeMismatch) {
  EXPECT_ERROR(Check("function f(x: int64, y: *int64): void { x = y; }"),
               "cannot implicitly convert Pointer(Scalar::kInt64) to "
               "Scalar::kInt64");
}

TEST_F(CheckerTest, ArrayAssignment) {
  Check(R"(
    function main(): void {
      var x: [1]int64;
      x[0] = 1;
    }
  )");
}

TEST_F(CheckerTest, ArrayPointerToSlice) {
  Check(R"(
    function main(): void {
      var x: [1]int64;
      var y: []int64;
      y = &x;
    }
  )");
}

TEST_F(CheckerTest, ImportExport) {
  WithSource("foo.aoc", "export var x: int64;");
  Check(R"(
    import "foo.aoc" as foo;

    function f(): void {
      foo.x = 1;
    }
  )");
}

}  // namespace
}  // namespace aoc2021
