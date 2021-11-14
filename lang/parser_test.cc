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
  WithSource(
      "f(\n"
      "-x,\n"
      "!x,\n"
      "~x,\n"
      "*x)");
  EXPECT_EQ(ParseExpression(),
            ast::Call(At(1, 2), ast::Name(At(1, 1), "f"),
                      {ast::Negate(At(2, 1), ast::Name(At(2, 2), "x")),
                       ast::LogicalNot(At(3, 1), ast::Name(At(3, 2), "x")),
                       ast::BitwiseNot(At(4, 1), ast::Name(At(4, 2), "x")),
                       ast::Dereference(At(5, 1), ast::Name(At(5, 2), "x"))}));
}

TEST_F(ParserTest, Product) {
  WithSource("a * b / c % d");
  EXPECT_EQ(
      ParseExpression(),
      ast::Modulo(At(1, 11),
                  ast::Divide(At(1, 7),
                              ast::Multiply(At(1, 3), ast::Name(At(1, 1), "a"),
                                            ast::Name(At(1, 5), "b")),
                              ast::Name(At(1, 9), "c")),
                  ast::Name(At(1, 13), "d")));
}

TEST_F(ParserTest, Sum) {
  WithSource("1 + 2 * 3 - 4");
  EXPECT_EQ(
      ParseExpression(),
      ast::Subtract(
          At(1, 11),
          ast::Add(At(1, 3), ast::IntegerLiteral(At(1, 1), 1),
                   ast::Multiply(At(1, 7), ast::IntegerLiteral(At(1, 5), 2),
                                 ast::IntegerLiteral(At(1, 9), 3))),
          ast::IntegerLiteral(At(1, 13), 4)));
}

TEST_F(ParserTest, Shift) {
  WithSource("a << b + c >> d");
  EXPECT_EQ(ParseExpression(),
            ast::ShiftRight(
                At(1, 12),
                ast::ShiftLeft(At(1, 3), ast::Name(At(1, 1), "a"),
                               ast::Add(At(1, 8), ast::Name(At(1, 6), "b"),
                                        ast::Name(At(1, 10), "c"))),
                ast::Name(At(1, 15), "d")));
}

TEST_F(ParserTest, Order) {
  WithSource(
      "a\n"
      "< b << c\n"
      "<= d\n"
      "> e\n"
      ">= f");
  EXPECT_EQ(ParseExpression(),
            ast::GreaterOrEqual(
                At(5, 1),
                ast::GreaterThan(
                    At(4, 1),
                    ast::LessOrEqual(
                        At(3, 1),
                        ast::LessThan(
                            At(2, 1), ast::Name(At(1, 1), "a"),
                            ast::ShiftLeft(At(2, 5), ast::Name(At(2, 3), "b"),
                                           ast::Name(At(2, 8), "c"))),
                        ast::Name(At(3, 4), "d")),
                    ast::Name(At(4, 3), "e")),
                ast::Name(At(5, 4), "f")));
}

TEST_F(ParserTest, Equal) {
  WithSource(
      "1\n"
      "== 2 < 3\n"
      "!= 4");
  EXPECT_EQ(
      ParseExpression(),
      ast::NotEqual(
          At(3, 1),
          ast::Equal(At(2, 1), ast::IntegerLiteral(At(1, 1), 1),
                     ast::LessThan(At(2, 6), ast::IntegerLiteral(At(2, 4), 2),
                                   ast::IntegerLiteral(At(2, 8), 3))),
          ast::IntegerLiteral(At(3, 4), 4)));
}

TEST_F(ParserTest, BitwiseAnd) {
  WithSource("a & b == c & d");
  EXPECT_EQ(ParseExpression(),
            ast::BitwiseAnd(
                At(1, 12),
                ast::BitwiseAnd(At(1, 3), ast::Name(At(1, 1), "a"),
                                ast::Equal(At(1, 7), ast::Name(At(1, 5), "b"),
                                           ast::Name(At(1, 10), "c"))),
                ast::Name(At(1, 14), "d")));
}

TEST_F(ParserTest, BitwiseXor) {
  WithSource("a ^ b & c ^ d");
  EXPECT_EQ(
      ParseExpression(),
      ast::BitwiseXor(
          At(1, 11),
          ast::BitwiseXor(At(1, 3), ast::Name(At(1, 1), "a"),
                          ast::BitwiseAnd(At(1, 7), ast::Name(At(1, 5), "b"),
                                          ast::Name(At(1, 9), "c"))),
          ast::Name(At(1, 13), "d")));
}

TEST_F(ParserTest, BitwiseOr) {
  WithSource("a | b ^ c | d");
  EXPECT_EQ(
      ParseExpression(),
      ast::BitwiseOr(
          At(1, 11),
          ast::BitwiseOr(At(1, 3), ast::Name(At(1, 1), "a"),
                         ast::BitwiseXor(At(1, 7), ast::Name(At(1, 5), "b"),
                                         ast::Name(At(1, 9), "c"))),
          ast::Name(At(1, 13), "d")));
}

TEST_F(ParserTest, Conjunction) {
  WithSource("a && b | c && d");
  EXPECT_EQ(
      ParseExpression(),
      ast::LogicalAnd(
          At(1, 12),
          ast::LogicalAnd(At(1, 3), ast::Name(At(1, 1), "a"),
                          ast::BitwiseOr(At(1, 8), ast::Name(At(1, 6), "b"),
                                         ast::Name(At(1, 10), "c"))),
          ast::Name(At(1, 15), "d")));
}

TEST_F(ParserTest, Disjunction) {
  WithSource("a || b && c || d");
  EXPECT_EQ(
      ParseExpression(),
      ast::LogicalOr(
          At(1, 13),
          ast::LogicalOr(At(1, 3), ast::Name(At(1, 1), "a"),
                         ast::LogicalAnd(At(1, 8), ast::Name(At(1, 6), "b"),
                                         ast::Name(At(1, 11), "c"))),
          ast::Name(At(1, 16), "d")));
}

TEST_F(ParserTest, Ternary) {
  WithSource(
      "a ?\n"
      "b ? c : d :\n"
      "e ? f : g");
  EXPECT_EQ(ParseExpression(),
            ast::TernaryExpression(
                At(1, 3), ast::Name(At(1, 1), "a"),
                ast::TernaryExpression(At(2, 3), ast::Name(At(2, 1), "b"),
                                       ast::Name(At(2, 5), "c"),
                                       ast::Name(At(2, 9), "d")),
                ast::TernaryExpression(At(3, 3), ast::Name(At(3, 1), "e"),
                                       ast::Name(At(3, 5), "f"),
                                       ast::Name(At(3, 9), "g"))));
}

}  // namespace
}  // namespace aoc2021
