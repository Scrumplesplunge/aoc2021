#include "parser.h"

#include "test.h"
#include <vector>

namespace aoc2021 {
namespace {

struct ParserTest : Test {
  void WithSource(std::string_view contents) {
    source.emplace("test_input.aoc", contents);
  }

  Location At(int line, int column) const {
    return Seek(source.value(), line, column).value();
  }

  ast::Expression ParseExpression() const {
    Parser parser(source.value());
    ast::Expression result = parser.ParseExpression();
    parser.ExpectEnd();
    return result;
  }

  std::optional<Source> source;
};

TEST_F(ParserTest, Name) {
  WithSource("x");
  EXPECT_EQ(ParseExpression(), ast::Name(At(1, 1), "x"));
}

TEST_F(ParserTest, IntegerLiteral) {
  WithSource("123");
  EXPECT_EQ(ParseExpression(), ast::IntegerLiteral(At(1, 1), 123));
}

TEST_F(ParserTest, ZeroPaddingIsDisallowed) {
  WithSource("0123");
  EXPECT_ERROR(ParseExpression(), "integer literals must not be 0-padded");
}

TEST_F(ParserTest, TrailingCharactersDisallowed) {
  WithSource("123foo");
  EXPECT_ERROR(ParseExpression(), "invalid integer literal");
}

TEST_F(ParserTest, MissingBracket) {
  WithSource("(1 + 2");
  EXPECT_ERROR(ParseExpression(), "expected ')'");
}

TEST_F(ParserTest, Call) {
  WithSource("f(1, x)");
  EXPECT_EQ(ParseExpression(), ast::Call(At(1, 2), ast::Name(At(1, 1), "f"),
                                         {ast::IntegerLiteral(At(1, 3), 1),
                                          ast::Name(At(1, 6), "x")}));
}

TEST_F(ParserTest, CallMissingBracket) {
  WithSource("f(1, x");
  EXPECT_ERROR(ParseExpression(), "expected ')'");
}

TEST_F(ParserTest, Index) {
  WithSource("a[b]");
  EXPECT_EQ(ParseExpression(), ast::Index(At(1, 2), ast::Name(At(1, 1), "a"),
                                          ast::Name(At(1, 3), "b")));
}

TEST_F(ParserTest, Prefix) {
  WithSource("-x");
  EXPECT_EQ(ParseExpression(), ast::Negate(At(1, 1), ast::Name(At(1, 2), "x")));
  WithSource("!x");
  EXPECT_EQ(ParseExpression(),
            ast::LogicalNot(At(1, 1), ast::Name(At(1, 2), "x")));
  WithSource("~x");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseNot(At(1, 1), ast::Name(At(1, 2), "x")));
  WithSource("*x");
  EXPECT_EQ(ParseExpression(),
            ast::Dereference(At(1, 1), ast::Name(At(1, 2), "x")));
  // Test that precedence is handled correctly: suffix expressions (such as the
  // brackets for a function call) should bind more tightly, and so -f() should
  // negate the result of a function call rather than calling the result of
  // a negation.
  WithSource("-f()");
  EXPECT_EQ(
      ParseExpression(),
      ast::Negate(At(1, 1), ast::Call(At(1, 3), ast::Name(At(1, 2), "f"), {})));
}

TEST_F(ParserTest, Product) {
  WithSource("a * b");
  EXPECT_EQ(ParseExpression(), ast::Multiply(At(1, 3), ast::Name(At(1, 1), "a"),
                                             ast::Name(At(1, 5), "b")));
  WithSource("a / b");
  EXPECT_EQ(ParseExpression(), ast::Divide(At(1, 3), ast::Name(At(1, 1), "a"),
                                           ast::Name(At(1, 5), "b")));
  WithSource("a % b");
  EXPECT_EQ(ParseExpression(), ast::Modulo(At(1, 3), ast::Name(At(1, 1), "a"),
                                           ast::Name(At(1, 5), "b")));
  // Test that expressions are left-associative: `a / b / c` should be
  // `(a / b) / c` rather than `a / (b / c)`.
  WithSource("a / b / c");
  EXPECT_EQ(ParseExpression(),
            ast::Divide(At(1, 7),
                        ast::Divide(At(1, 3), ast::Name(At(1, 1), "a"),
                                    ast::Name(At(1, 5), "b")),
                        ast::Name(At(1, 9), "c")));
  // Test that precedence is handled correctly: unary expressions should bind
  // more tightly.
  WithSource("-a * b");
  EXPECT_EQ(
      ParseExpression(),
      ast::Multiply(At(1, 4), ast::Negate(At(1, 1), ast::Name(At(1, 2), "a")),
                    ast::Name(At(1, 6), "b")));
}

TEST_F(ParserTest, Sum) {
  WithSource("a + b");
  EXPECT_EQ(ParseExpression(), ast::Add(At(1, 3), ast::Name(At(1, 1), "a"),
                                        ast::Name(At(1, 5), "b")));
  WithSource("a - b");
  EXPECT_EQ(ParseExpression(), ast::Subtract(At(1, 3), ast::Name(At(1, 1), "a"),
                                             ast::Name(At(1, 5), "b")));
  // Test that expressions are left-associative: `a + b - c` should be
  // `(a + b) - c` rather than `a + (b - c)`.
  WithSource("a + b - c");
  EXPECT_EQ(ParseExpression(),
            ast::Subtract(At(1, 7),
                          ast::Add(At(1, 3), ast::Name(At(1, 1), "a"),
                                   ast::Name(At(1, 5), "b")),
                          ast::Name(At(1, 9), "c")));
  // Test that precedence is handled correctly: product expressions should bind
  // more tightly.
  WithSource("a + b * c");
  EXPECT_EQ(ParseExpression(),
            ast::Add(At(1, 3), ast::Name(At(1, 1), "a"),
                     ast::Multiply(At(1, 7), ast::Name(At(1, 5), "b"),
                                   ast::Name(At(1, 9), "c"))));
}

TEST_F(ParserTest, Shift) {
  WithSource("a << b");
  EXPECT_EQ(ParseExpression(),
            ast::ShiftLeft(At(1, 3), ast::Name(At(1, 1), "a"),
                           ast::Name(At(1, 6), "b")));
  WithSource("a >> b");
  EXPECT_EQ(ParseExpression(),
            ast::ShiftRight(At(1, 3), ast::Name(At(1, 1), "a"),
                            ast::Name(At(1, 6), "b")));
  // Test that expressions are left-associative: `a << b << c` should be
  // `(a << b) << c` rather than `a << (b << c)`.
  WithSource("a << b << c");
  EXPECT_EQ(ParseExpression(),
            ast::ShiftLeft(At(1, 8),
                           ast::ShiftLeft(At(1, 3), ast::Name(At(1, 1), "a"),
                                          ast::Name(At(1, 6), "b")),
                           ast::Name(At(1, 11), "c")));
  // Test that precedence is handled correctly: sum expressions should bind
  // more tightly.
  WithSource("a << b + c");
  EXPECT_EQ(ParseExpression(),
            ast::ShiftLeft(At(1, 3), ast::Name(At(1, 1), "a"),
                           ast::Add(At(1, 8), ast::Name(At(1, 6), "b"),
                                    ast::Name(At(1, 10), "c"))));
}

TEST_F(ParserTest, Order) {
  WithSource("a < b");
  EXPECT_EQ(ParseExpression(), ast::LessThan(At(1, 3), ast::Name(At(1, 1), "a"),
                                             ast::Name(At(1, 5), "b")));
  WithSource("a <= b");
  EXPECT_EQ(ParseExpression(),
            ast::LessOrEqual(At(1, 3), ast::Name(At(1, 1), "a"),
                             ast::Name(At(1, 6), "b")));
  WithSource("a > b");
  EXPECT_EQ(ParseExpression(),
            ast::GreaterThan(At(1, 3), ast::Name(At(1, 1), "a"),
                             ast::Name(At(1, 5), "b")));
  WithSource("a >= b");
  EXPECT_EQ(ParseExpression(),
            ast::GreaterOrEqual(At(1, 3), ast::Name(At(1, 1), "a"),
                                ast::Name(At(1, 6), "b")));
  // Test that expressions are left-associative: `a < b < c` should be
  // `(a < b) < c` rather than `a < (b < c)`.
  WithSource("a < b < c");
  EXPECT_EQ(ParseExpression(),
            ast::LessThan(At(1, 7),
                          ast::LessThan(At(1, 3), ast::Name(At(1, 1), "a"),
                                        ast::Name(At(1, 5), "b")),
                          ast::Name(At(1, 9), "c")));
  // Test that precedence is handled correctly: shift expressions should bind
  // more tightly.
  WithSource("a < b << c");
  EXPECT_EQ(ParseExpression(),
            ast::LessThan(At(1, 3), ast::Name(At(1, 1), "a"),
                          ast::ShiftLeft(At(1, 7), ast::Name(At(1, 5), "b"),
                                         ast::Name(At(1, 10), "c"))));
}

TEST_F(ParserTest, Equal) {
  WithSource("a == b");
  EXPECT_EQ(ParseExpression(), ast::Equal(At(1, 3), ast::Name(At(1, 1), "a"),
                                          ast::Name(At(1, 6), "b")));
  WithSource("a != b");
  EXPECT_EQ(ParseExpression(), ast::NotEqual(At(1, 3), ast::Name(At(1, 1), "a"),
                                             ast::Name(At(1, 6), "b")));
  // Test that expressions are left-associative: `a == b == c` should be
  // `(a == b) == c` rather than `a == (b == c)`.
  WithSource("a == b == c");
  EXPECT_EQ(ParseExpression(),
            ast::Equal(At(1, 8),
                       ast::Equal(At(1, 3), ast::Name(At(1, 1), "a"),
                                  ast::Name(At(1, 6), "b")),
                       ast::Name(At(1, 11), "c")));
  // Test that precedence is handled correctly: order expressions should bind
  // more tightly.
  WithSource("a == b < c");
  EXPECT_EQ(ParseExpression(),
            ast::Equal(At(1, 3), ast::Name(At(1, 1), "a"),
                       ast::LessThan(At(1, 8), ast::Name(At(1, 6), "b"),
                                     ast::Name(At(1, 10), "c"))));
}

TEST_F(ParserTest, BitwiseAnd) {
  WithSource("a & b");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseAnd(At(1, 3), ast::Name(At(1, 1), "a"),
                            ast::Name(At(1, 5), "b")));
  // Test that precedence is handled correctly: equality expressions should bind
  // more tightly.
  WithSource("a & b == c");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseAnd(At(1, 3), ast::Name(At(1, 1), "a"),
                            ast::Equal(At(1, 7), ast::Name(At(1, 5), "b"),
                                       ast::Name(At(1, 10), "c"))));
}

TEST_F(ParserTest, BitwiseXor) {
  WithSource("a ^ b");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseXor(At(1, 3), ast::Name(At(1, 1), "a"),
                            ast::Name(At(1, 5), "b")));
  // Test that precedence is handled correctly: bitwise and expressions should
  // bind more tightly.
  WithSource("a ^ b & c");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseXor(At(1, 3), ast::Name(At(1, 1), "a"),
                            ast::BitwiseAnd(At(1, 7), ast::Name(At(1, 5), "b"),
                                            ast::Name(At(1, 9), "c"))));
}

TEST_F(ParserTest, BitwiseOr) {
  WithSource("a | b");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseOr(At(1, 3), ast::Name(At(1, 1), "a"),
                           ast::Name(At(1, 5), "b")));
  // Test that precedence is handled correctly: bitwise xor expressions should
  // bind more tightly.
  WithSource("a | b ^ c");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseOr(At(1, 3), ast::Name(At(1, 1), "a"),
                           ast::BitwiseXor(At(1, 7), ast::Name(At(1, 5), "b"),
                                           ast::Name(At(1, 9), "c"))));
}

TEST_F(ParserTest, Conjunction) {
  WithSource("a && b");
  EXPECT_EQ(ParseExpression(),
            ast::LogicalAnd(At(1, 3), ast::Name(At(1, 1), "a"),
                            ast::Name(At(1, 6), "b")));
  // Test that precedence is handled correctly: bitwise or expressions should
  // bind more tightly.
  WithSource("a && b | c");
  EXPECT_EQ(ParseExpression(),
            ast::LogicalAnd(At(1, 3), ast::Name(At(1, 1), "a"),
                            ast::BitwiseOr(At(1, 8), ast::Name(At(1, 6), "b"),
                                           ast::Name(At(1, 10), "c"))));
}

TEST_F(ParserTest, Disjunction) {
  WithSource("a || b");
  EXPECT_EQ(ParseExpression(),
            ast::LogicalOr(At(1, 3), ast::Name(At(1, 1), "a"),
                           ast::Name(At(1, 6), "b")));
  // Test that precedence is handled correctly: logical and expressions should
  // bind more tightly.
  WithSource("a || b && c");
  EXPECT_EQ(ParseExpression(),
            ast::LogicalOr(At(1, 3), ast::Name(At(1, 1), "a"),
                           ast::LogicalAnd(At(1, 8), ast::Name(At(1, 6), "b"),
                                           ast::Name(At(1, 11), "c"))));
}

TEST_F(ParserTest, Ternary) {
  WithSource("a ? b : c");
  EXPECT_EQ(ParseExpression(),
            ast::TernaryExpression(At(1, 3), ast::Name(At(1, 1), "a"),
                                   ast::Name(At(1, 5), "b"),
                                   ast::Name(At(1, 9), "c")));
  // Ternary expression as the first argument of a ternary expression.
  WithSource("c1 ? c2 ? v1 : v2\n"
             "   : v3");
  EXPECT_EQ(ParseExpression(),
            ast::TernaryExpression(
                At(1, 4), ast::Name(At(1, 1), "c1"),
                ast::TernaryExpression(At(1, 9), ast::Name(At(1, 6), "c2"),
                                       ast::Name(At(1, 11), "v1"),
                                       ast::Name(At(1, 16), "v2")),
                ast::Name(At(2, 6), "v3")));
  // Ternary expression as the second argument of a ternary expression.
  WithSource(
      "c1 ? v1 :\n"
      "c2 ? v2 :\n"
      "v3");
  EXPECT_EQ(ParseExpression(),
            ast::TernaryExpression(
                At(1, 4), ast::Name(At(1, 1), "c1"), ast::Name(At(1, 6), "v1"),
                ast::TernaryExpression(At(2, 4), ast::Name(At(2, 1), "c2"),
                                       ast::Name(At(2, 6), "v2"),
                                       ast::Name(At(3, 1), "v3"))));
}

}  // namespace
}  // namespace aoc2021
