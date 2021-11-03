#ifndef PARSER_H_
#define PARSER_H_

#include "ast.h"
#include "source.h"

#include <memory>
#include <string_view>
#include <string>

namespace aoc2021 {

// A parser for an aoclang source file.
class Parser {
 public:
  explicit Parser(const Source& source) noexcept : reader_(source) {}

  AnyExpression ParseExpression();

 private:
  Name ParseName();
  IntegerLiteral ParseIntegerLiteral();
  AnyExpression ParseTerm();
  AnyExpression ParseSuffix();
  AnyExpression ParsePrefix();
  AnyExpression ParseProduct();
  AnyExpression ParseSum();
  AnyExpression ParseShift();
  AnyExpression ParseOrder();
  AnyExpression ParseEqual();
  AnyExpression ParseBitwiseAnd();
  AnyExpression ParseBitwiseXor();
  AnyExpression ParseBitwiseOr();
  AnyExpression ParseConjunction();
  AnyExpression ParseDisjunction();
  AnyExpression ParseTernary();

  std::string_view PeekWord() const noexcept;
  std::string_view PeekOperator() const noexcept;

  bool ConsumeOperator(std::string_view value) noexcept;

  ParseError Error(std::string text) const noexcept {
    return ParseError({ErrorMessage(std::move(text))});
  }

  Message ErrorMessage(std::string text) const noexcept;

  void SkipWhitespace();
  void SkipWhitespaceAndComments();

  Reader reader_;
};

}  // namespace aoc2021

#endif  // PARSER_H_
