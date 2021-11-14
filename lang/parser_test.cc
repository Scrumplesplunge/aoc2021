#include "parser.h"

#include "test.h"
#include <vector>

namespace aoc2021 {
namespace {

TEST(Name) {
  Source source("test_input.aoc", "x");
  EXPECT_EQ(Parser(source).ParseExpression(),
            ast::Name(Seek(source, 1, 1).value(), "x"));
}

TEST(IntegerLiteral) {
  Source source("test_input.aoc", "123");
  EXPECT_EQ(Parser(source).ParseExpression(),
            ast::IntegerLiteral(Seek(source, 1, 1).value(), 123));
}

TEST(ZeroPaddingIsDisallowed) {
  Source source("test_input.aoc", "0123");
  EXPECT_ERROR(Parser(source).ParseExpression(),
               "integer literals must not be 0-padded");
}

TEST(TrailingCharactersDisallowed) {
  Source source("test_input.aoc", "123foo");
  EXPECT_ERROR(Parser(source).ParseExpression(), "invalid integer literal");
}

TEST(MissingBracket) {
  Source source("test_input.aoc", "(1 + 2");
  EXPECT_ERROR(Parser(source).ParseExpression(), "expected ')'");
}

TEST(Call) {
  Source source("test_input.aoc", "f(1, x)");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::Call(at(1, 2), ast::Name(at(1, 1), "f"),
                {ast::IntegerLiteral(at(1, 3), 1), ast::Name(at(1, 6), "x")}));
}

TEST(CallMissingBracket) {
  Source source("test_input.aoc", "f(1, x");
  EXPECT_ERROR(Parser(source).ParseExpression(), "expected ')'");
}

TEST(Index) {
  Source source("test_input.aoc", "a[b]");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::Index(at(1, 2), ast::Name(at(1, 1), "a"), ast::Name(at(1, 3), "b")));
}

TEST(Prefix) {
  Source source("test_input.aoc",
                "f(\n"
                "-x,\n"
                "!x,\n"
                "~x,\n"
                "*x)");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(Parser(source).ParseExpression(),
            ast::Call(at(1, 2), ast::Name(at(1, 1), "f"),
                      {ast::Negate(at(2, 1), ast::Name(at(2, 2), "x")),
                       ast::LogicalNot(at(3, 1), ast::Name(at(3, 2), "x")),
                       ast::BitwiseNot(at(4, 1), ast::Name(at(4, 2), "x")),
                       ast::Dereference(at(5, 1), ast::Name(at(5, 2), "x"))}));
}

TEST(Product) {
  Source source("test_input.aoc", "a * b / c % d");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::Modulo(at(1, 11),
                  ast::Divide(at(1, 7),
                              ast::Multiply(at(1, 3), ast::Name(at(1, 1), "a"),
                                            ast::Name(at(1, 5), "b")),
                              ast::Name(at(1, 9), "c")),
                  ast::Name(at(1, 13), "d")));
}

TEST(Sum) {
  Source source("test_input.aoc", "1 + 2 * 3 - 4");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::Subtract(
          at(1, 11),
          ast::Add(at(1, 3), ast::IntegerLiteral(at(1, 1), 1),
                   ast::Multiply(at(1, 7), ast::IntegerLiteral(at(1, 5), 2),
                                 ast::IntegerLiteral(at(1, 9), 3))),
          ast::IntegerLiteral(at(1, 13), 4)));
}

TEST(Shift) {
  Source source("test_input.aoc", "a << b + c >> d");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(Parser(source).ParseExpression(),
            ast::ShiftRight(
                at(1, 12),
                ast::ShiftLeft(at(1, 3), ast::Name(at(1, 1), "a"),
                               ast::Add(at(1, 8), ast::Name(at(1, 6), "b"),
                                        ast::Name(at(1, 10), "c"))),
                ast::Name(at(1, 15), "d")));
}

TEST(Order) {
  Source source("test_input.aoc",
                "a\n"
                "< b << c\n"
                "<= d\n"
                "> e\n"
                ">= f");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(Parser(source).ParseExpression(),
            ast::GreaterOrEqual(
                at(5, 1),
                ast::GreaterThan(
                    at(4, 1),
                    ast::LessOrEqual(
                        at(3, 1),
                        ast::LessThan(
                            at(2, 1), ast::Name(at(1, 1), "a"),
                            ast::ShiftLeft(at(2, 5), ast::Name(at(2, 3), "b"),
                                           ast::Name(at(2, 8), "c"))),
                        ast::Name(at(3, 4), "d")),
                    ast::Name(at(4, 3), "e")),
                ast::Name(at(5, 4), "f")));
}

TEST(Equal) {
  Source source("test_input.aoc",
                "1\n"
                "== 2 < 3\n"
                "!= 4");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::NotEqual(
          at(3, 1),
          ast::Equal(at(2, 1), ast::IntegerLiteral(at(1, 1), 1),
                     ast::LessThan(at(2, 6), ast::IntegerLiteral(at(2, 4), 2),
                                   ast::IntegerLiteral(at(2, 8), 3))),
          ast::IntegerLiteral(at(3, 4), 4)));
}

TEST(BitwiseAnd) {
  Source source("test_input.aoc", "a & b == c & d");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(Parser(source).ParseExpression(),
            ast::BitwiseAnd(
                at(1, 12),
                ast::BitwiseAnd(at(1, 3), ast::Name(at(1, 1), "a"),
                                ast::Equal(at(1, 7), ast::Name(at(1, 5), "b"),
                                           ast::Name(at(1, 10), "c"))),
                ast::Name(at(1, 14), "d")));
}

TEST(BitwiseXor) {
  Source source("test_input.aoc", "a ^ b & c ^ d");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::BitwiseXor(
          at(1, 11),
          ast::BitwiseXor(at(1, 3), ast::Name(at(1, 1), "a"),
                          ast::BitwiseAnd(at(1, 7), ast::Name(at(1, 5), "b"),
                                          ast::Name(at(1, 9), "c"))),
          ast::Name(at(1, 13), "d")));
}

TEST(BitwiseOr) {
  Source source("test_input.aoc", "a | b ^ c | d");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::BitwiseOr(
          at(1, 11),
          ast::BitwiseOr(at(1, 3), ast::Name(at(1, 1), "a"),
                          ast::BitwiseXor(at(1, 7), ast::Name(at(1, 5), "b"),
                                          ast::Name(at(1, 9), "c"))),
          ast::Name(at(1, 13), "d")));
}

TEST(Conjunction) {
  Source source("test_input.aoc", "a && b | c && d");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::LogicalAnd(
          at(1, 12),
          ast::LogicalAnd(at(1, 3), ast::Name(at(1, 1), "a"),
                          ast::BitwiseOr(at(1, 8), ast::Name(at(1, 6), "b"),
                                         ast::Name(at(1, 10), "c"))),
          ast::Name(at(1, 15), "d")));
}

TEST(Disjunction) {
  Source source("test_input.aoc", "a || b && c || d");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(
      Parser(source).ParseExpression(),
      ast::LogicalOr(
          at(1, 13),
          ast::LogicalOr(at(1, 3), ast::Name(at(1, 1), "a"),
                         ast::LogicalAnd(at(1, 8), ast::Name(at(1, 6), "b"),
                                         ast::Name(at(1, 11), "c"))),
          ast::Name(at(1, 16), "d")));
}

TEST(Ternary) {
  Source source("test_input.aoc",
                "a ?\n"
                "b ? c : d :\n"
                "e ? f : g");
  auto at = [&](int line, int column) {
    return Seek(source, line, column).value();
  };
  EXPECT_EQ(Parser(source).ParseExpression(),
            ast::TernaryExpression(
                at(1, 3), ast::Name(at(1, 1), "a"),
                ast::TernaryExpression(at(2, 3), ast::Name(at(2, 1), "b"),
                                       ast::Name(at(2, 5), "c"),
                                       ast::Name(at(2, 9), "d")),
                ast::TernaryExpression(at(3, 3), ast::Name(at(3, 1), "e"),
                                       ast::Name(at(3, 5), "f"),
                                       ast::Name(at(3, 9), "g"))));
}

}  // namespace
}  // namespace aoc2021
