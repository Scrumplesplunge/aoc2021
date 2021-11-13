#ifndef PARSER_H_
#define PARSER_H_

#include "ast.h"
#include "source.h"

#include <memory>
#include <string_view>
#include <string>

namespace aoc2021 {

struct ParseError : public SourceError {
  using SourceError::SourceError;
};

// A parser for an aoclang source file.
class Parser {
 public:
  explicit Parser(const Source& source) noexcept : reader_(source) {}

  ast::Expression ParseExpression();
  ast::AnyStatement ParseStatement();
  std::vector<ast::AnyStatement> ParseProgram();

 private:
  ast::Name ParseName();
  ast::IntegerLiteral ParseIntegerLiteral();
  ast::Expression ParseTerm();
  ast::Expression ParseSuffix();
  ast::Expression ParsePrefix();
  ast::Expression ParseProduct();
  ast::Expression ParseSum();
  ast::Expression ParseShift();
  ast::Expression ParseOrder();
  ast::Expression ParseEqual();
  ast::Expression ParseBitwiseAnd();
  ast::Expression ParseBitwiseXor();
  ast::Expression ParseBitwiseOr();
  ast::Expression ParseConjunction();
  ast::Expression ParseDisjunction();
  ast::Expression ParseTernary();

  std::vector<ast::AnyStatement> ParseBlock();

  ast::AnyStatement ParseBreak();
  ast::AnyStatement ParseContinue();
  ast::AnyStatement ParseFunctionDefinition();
  ast::AnyStatement ParseIf();
  ast::AnyStatement ParseReturn();
  ast::AnyStatement ParseDeclaration();
  ast::AnyStatement ParseWhile();

  std::string_view PeekWord() const noexcept;
  std::string_view PeekOperator() const noexcept;

  bool ConsumeWord(std::string_view value) noexcept;
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
